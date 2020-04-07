package com.github.andyglow.jsonschema

import com.github.andyglow.json.ToValue

import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

object SchemaMacro {

  def impl[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[json.Schema[T]] = {
    import c.universe._

    val jsonPkg     = q"_root_.json"
    val intJsonPkg  = q"_root_.com.github.andyglow.json"
    val schemaObj   = q"$jsonPkg.Schema"

    val subject               = weakTypeOf[T]
    val optionTpe             = weakTypeOf[Option[_]]
    val toValueTpe            = weakTypeOf[ToValue[_]]
    val setTpe                = weakTypeOf[Set[_]]
    val schemaTypeConstructor = weakTypeOf[json.Schema[_]].typeConstructor
    val schemaSubject         = appliedType(schemaTypeConstructor, subject)
    val predefTypeConstructor = weakTypeOf[json.Schema.Predefined[_]].typeConstructor
    val predefSubject         = appliedType(predefTypeConstructor, subject)

    def resolve(tpe: Type, stack: List[Type]): Tree = {
      if (stack contains tpe) c.error(c.enclosingPosition, s"cyclic dependency for $tpe")

      def genTree: Tree = tpe match {
        case SealedEnum(names)                  => SealedEnum.gen(tpe, names)
        case SealedClasses(subTypes)            => SealedClasses.gen(tpe, subTypes, stack)
        case CaseClass(fields)                  => CaseClass.gen(fields, tpe, stack)
        case ValueClass(innerType)              => ValueClass.gen(innerType, tpe, stack)
        case x if x <:< typeOf[Map[String, _]]  => StringMap.gen(x, stack)
        case x if x <:< typeOf[Map[Int, _]]     => IntMap.gen(x, stack)
        case x if x <:< typeOf[Array[_]]        => Arr.gen(x, stack)
        case x if x <:< typeOf[Iterable[_]]     => Arr.gen(x, stack)

        case _ =>
          c.error(c.enclosingPosition, s"schema for $tpe is not supported, ${stack mkString " :: "}")
          q"null"
      }

      Implicit.getOrElse(tpe, genTree)
    }

    object Implicit {

      def getOrElse(tpe: Type, gen: => Tree): Tree = {
        val sType = appliedType(schemaTypeConstructor, tpe)
        val pType = appliedType(predefTypeConstructor, tpe)
        println(s"resolve ${show(pType)}")
        c.inferImplicitValue(pType) match {
          case EmptyTree => println(s"Empty, so resolve ${show(sType)}"); c.inferImplicitValue(sType) match {
            case EmptyTree => println(s"Empty, so generate"); gen
            case x         => println(s"Found Schema, so make a ref"); q"""$schemaObj.`ref`[$tpe]($jsonPkg.Json.sig[$tpe].signature, $x)"""
          }
          case x         => println(s"Found Predefined, so point to it"); q"$x.schema"
        }
      }
    }

    object SealedEnum {

      def unapply(tpe: Type): Option[Set[Tree]] = {

        if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed) {
          val instances = tpe.typeSymbol.asClass.knownDirectSubclasses
          val toValueTree = c.inferImplicitValue(
            appliedType(toValueTpe, tpe),
            silent = true,
            withMacrosDisabled = true)

          if (instances forall { i => val c = i.asClass; c.isModuleClass}) {
            if (toValueTree.nonEmpty) {
              Some(instances collect {
                case i: ClassSymbol =>
                  val caseObj = i.owner.asClass.toType.decls.find { d =>
                    d.name == i.name.toTermName
                  } getOrElse NoSymbol

                  q"$toValueTree($caseObj)"
              })
            } else {
              Some(instances map { i => q"$intJsonPkg.Value.str(${i.name.decodedName.toString})" })
            }
          } else
            None
        } else
          None
      }

      def gen(tpe: Type, symbols: Set[Tree]): Tree = {
        q"$schemaObj.`enum`[$tpe]($symbols)"
      }
    }

    object SealedClasses {

      def unapply(tpe: Type): Option[Set[Type]] = {

        def isSealed(x: Type): Boolean = {
          val s = x.typeSymbol
          s.isClass && s.asClass.isSealed
        }

        def isSupportedLeafType(x: Type): Boolean = {
          val s = x.typeSymbol
          s.isClass && !s.isModuleClass && s.asClass.isCaseClass
        }

        def substituteTypes(x: Type, from: List[Symbol], to: List[Type]): Type =
          try x.substituteTypes(from, to) catch { case NonFatal(_) =>
            c.abort(c.enclosingPosition, s"Cannot resolve generic type(s) for `$x`. Please provide a custom implicitly accessible json.Schema for it.")
          }

        // BORROWED:
        // https://github.com/plokhotnyuk/jsoniter-scala/blob/3612fddf19a8ce23ac973d71e85ef02f79c06fff/jsoniter-scala-macros/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L351-L365
        def collectRecursively(x: Type): Seq[Type] =
          if (x.typeSymbol.isClass) {
            val leafs = x.typeSymbol.asClass.knownDirectSubclasses.toSeq flatMap { s =>
              val cs = s.asClass
              val subTpe = if (cs.typeParams.isEmpty) cs.toType else substituteTypes(cs.toType, cs.typeParams, x.typeArgs)
              if (isSealed(subTpe)) collectRecursively(subTpe)
              else if (isSupportedLeafType(subTpe)) Seq(subTpe)
              else c.abort(c.enclosingPosition, "Only Scala case classes are supported for ADT leaf classes. Please consider using of " +
                s"them for ADT with base '$x' or provide a custom implicitly accessible json.Schema for the ADT base.")
            }
            if (isSupportedLeafType(x)) leafs :+ x else leafs
          } else Seq.empty

        if (isSealed(tpe)) {
          val instances = collectRecursively(tpe)
          Some(instances.toSet)
        } else
          None
      }

      def gen(tpe: Type, subTypes: Set[Type], stack: List[Type]): Tree = {
        val trees = subTypes collect {
          case CaseClass(fields)     => CaseClass.gen(fields, tpe, stack)
          case ValueClass(innerType) => ValueClass.gen(innerType, tpe, stack)
        }

        q"$schemaObj.`oneof`[$tpe]($trees)"
      }
    }

    object CaseClass {

      // TODO: add support for case classes defined in method body

      final def lookupCompanionOf(clazz: Symbol): Symbol = clazz.companion

      def possibleApplyMethodsOf(subjectCompanion: Symbol): List[MethodSymbol] = {
        val subjectCompanionTpe = subjectCompanion.typeSignature

        subjectCompanionTpe.decl(TermName("apply")) match {

          case NoSymbol =>
            c.abort(c.enclosingPosition, s"No apply function found for ${subjectCompanion.fullName}")

          case x => x.asTerm.alternatives flatMap { apply =>
            val method = apply.asMethod

            def areAllImplicit(pss: List[List[Symbol]]): Boolean = pss forall {
              case p :: _ => p.isImplicit
              case _      => false
            }

            method.paramLists match {
              case ps :: pss if ps.nonEmpty && areAllImplicit(pss) => List(method)
              case _ => List.empty
            }
          }
        }
      }

      def applyMethod(subjectCompanion: Symbol): Option[MethodSymbol] =
        possibleApplyMethodsOf(subjectCompanion).headOption

      case class Field(
        name: TermName,
        tpe: Type,
        effectiveTpe: Type,
        annotations: List[Annotation],
        default: Option[Tree],
        isOption: Boolean) {

        def hasDefault: Boolean = default.isDefined
      }

      def fieldMap(tpe: Type): Seq[Field] = {

        val annotationMap = tpe.decls.collect {

          case s: MethodSymbol if s.isCaseAccessor =>
            // workaround: force loading annotations
            s.typeSignature
            s.accessed.annotations.foreach(_.tree.tpe)

            s.name.toString.trim -> s.accessed.annotations
        }.toMap

        val subjectCompanionSym = tpe.typeSymbol
        val subjectCompanion    = lookupCompanionOf(subjectCompanionSym)

        def toField(fieldSym: TermSymbol, i: Int): Field = {
          val name        = fieldSym.name.toString.trim
          val fieldTpe    = fieldSym.typeSignature
          val isOption    = fieldTpe <:< optionTpe
          val hasDefault  = fieldSym.isParamWithDefault
          val toV         = c.inferImplicitValue(appliedType(toValueTpe, fieldTpe))
          val default     = if (hasDefault) {
            val getter = TermName("apply$default$" + (i + 1))
            if (toV.nonEmpty) Some(q"Some($toV($subjectCompanion.$getter))") else {
              c.error(c.enclosingPosition, s"Can't infer a json value for $name")
              None
            }
          } else
            None

          Field(
            name          = TermName(name),
            tpe           = fieldTpe,
            effectiveTpe  = if (isOption) fieldTpe.typeArgs.head else fieldTpe,
            annotations   = annotationMap.getOrElse(name, List.empty),
            default       = default,
            isOption      = isOption)
        }

        val fields = applyMethod(subjectCompanion) flatMap { method =>
          method.paramLists.headOption map { params =>
            params.map { _.asTerm }.zipWithIndex map { case (f, i) => toField(f, i) }
          }
        }

        fields getOrElse Seq.empty
      }

      def unapply(tpe: Type): Option[Seq[CaseClass.Field]] = {
        val symbol = tpe.typeSymbol

        if (symbol.isClass) {
          val clazz = symbol.asClass
          if (clazz.isCaseClass) {
            if (clazz.isDerivedValueClass) None else Some(fieldMap(tpe))
          } else
            None
        } else
          None
      }

      def gen(fieldMap: Seq[CaseClass.Field], tpe: Type, stack: List[Type]): Tree = {
        val obj = q"$schemaObj.`object`"
        val fields = fieldMap map { f =>
          val name      = f.name.decodedName.toString
          val jsonType  = resolve(f.effectiveTpe, if (f.isOption) stack else tpe +: stack)

          f.default map { d =>
            q"$obj.Field.fromJson[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault }, default = $d)"
          } getOrElse {
            q"$obj.Field[${f.effectiveTpe}](name = $name, tpe = $jsonType, required = ${ !f.isOption && !f.hasDefault })"
          }
        }

        q"$obj[$tpe](..$fields)"
      }
    }

    object ValueClass {

      def unapply(x: Type): Option[Type] = {
        val symbol = x.typeSymbol

        if (symbol.isClass) {
          val clazz = symbol.asClass
          if (clazz.isCaseClass) {
            if (clazz.isDerivedValueClass) Some {
              clazz.primaryConstructor.asMethod.paramLists.head.head.typeSignature
            } else
              None
          } else
            None
        } else
          None
      }

      def gen(innerType: Type, tpe: Type, stack: List[Type]): Tree = {
        val x = resolve(innerType, tpe +: stack)
        val z = x match {
          case q"""$c[$t](..$args)""" => q"$c[$tpe](..$args)"
          case x => val st = appliedType(schemaTypeConstructor, tpe); q"$x.asInstanceOf[$st]"
        }
        println(s"VC.gen ${tpe} [${innerType}] => ${show(x)} => ${show(z)}")
        z
      }
    }

    object IntMap {

      def gen(tpe: Type, stack: List[Type]): Tree = {
        val componentType = tpe.typeArgs.tail.head
        val componentJsonType = resolve(componentType, tpe +: stack)

        q"""$schemaObj.`int-map`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
      }
    }

    object StringMap {

      def gen(tpe: Type, stack: List[Type]): Tree = {
        val componentType = tpe.typeArgs.tail.head
        val componentJsonType = resolve(componentType, tpe +: stack)

        q"""$schemaObj.`string-map`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
      }
    }

    object Arr {

      def gen(tpe: Type, stack: List[Type]): Tree = {
        val componentType     = tpe.typeArgs.head
        val componentJsonType = resolve(componentType, tpe +: stack)
        val isSet             = tpe <:< setTpe

        if (isSet)
          q"""$schemaObj.`set`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
        else
          q"""$schemaObj.`array`[$componentType, ${tpe.typeConstructor}]($componentJsonType)"""
      }
    }

    val out = {
      val pd = c.inferImplicitValue(predefSubject)
      if (pd.isEmpty) pd else q"$pd.schema"
    } orElse c.inferImplicitValue(schemaSubject) orElse resolve(subject, Nil)

    c.info(c.enclosingPosition, show(out), force = false)

    c.Expr[json.Schema[T]](out)
  }
}
