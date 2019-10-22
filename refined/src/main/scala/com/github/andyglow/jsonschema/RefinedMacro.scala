package com.github.andyglow.jsonschema

import eu.timepit.refined.numeric.Positive

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object RefinedMacro {

  def refined[T]: json.Schema[T] = macro impl[T]

  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[json.Schema[T]] = {
    import c.universe._
    val t = weakTypeTag[T].tpe.dealias

    val `Refined`   = typeOf[eu.timepit.refined.api.Refined[_, _]].typeSymbol

    object RN {

      def unapply(t: Type): Option[(Symbol, String)] =
        t match {
          case TypeRef(_, `Refined`, List(TypeRef(_, t, _), TypeRef(_, p, _))) => Some((t, p.fullName.drop(19)))
          case _ => None
        }
    }

    object RC {

      def unapply(t: Type): Option[(Symbol, String, Constant)] =
        t match {
          case TypeRef(_, `Refined`, List(TypeRef(_, t, _), TypeRef(_, p, List(ConstantType(pp))))) => Some((t, p.fullName.drop(19), pp))
          case _ => None
        }
    }

    object RT {

      def unapply(t: Type): Option[(Symbol, String, List[Type])] =
        t match {
          case TypeRef(_, `Refined`, List(TypeRef(_, t, _), TypeRef(_, p, pp))) => Some((t, p.fullName.drop(19), pp.collect{ case TypeRef(_, x, _) => x.typeSignature }))
          case _ => None
        }
    }

    val dbg = c.info(c.enclosingPosition, _, force = true)

//    dbg(showRaw(t))

    sealed trait Pred {
      def t: Type
      def tree: Tree
    }

    object Pred {
      case class Pos(t: Type) extends Pred { def tree = q"`number`[$t]() withValidation ( `minimum` := 0 )" }
      case class Neg(t: Type) extends Pred { def tree = q"`number`[$t]() withValidation ( `maximum` := 0 )" }
      case class Ge(t: Type, v: Any) extends Pred { def tree = q"`number`[$t]() withValidation ( `minimum` := ${v.asInstanceOf[Number].doubleValue() } )" }
      case class Le(t: Type, v: Any) extends Pred { def tree = q"`number`[$t]() withValidation ( `maximum` := ${v.asInstanceOf[Number].doubleValue() } )" }

      case class Not(p: Pred) extends Pred {
        def t = p.t
        def tree: Tree = {

          def compile(p: Pred): Pred = p match {
            case Pos(t) => Neg(t)
            case Neg(t) => Pos(t)
            case Ge(t, v) => Le(t, v)
            case Le(t, v) => Ge(t, v)
            case p: Not => compile(p)
            case p => p
          }

          compile(p).tree
        }
      }
//
//      case class Or(l: Pred, r: Pred) extends Pred { def t = l.t }
//      case class And(l: Pred, r: Pred) extends Pred { def t = l.t }
    }

    def scan(t: Type): Pred = {
      import Pred._

      t match {
        case RN(t, "numeric.Positive")              => Pos(t.asType.toType)
        case RN(t, "numeric.Negative")              => Neg(t.asType.toType)
        case RC(t, "numeric.Greater" , Constant(x)) => Ge(t.asType.toType, x)
        case RC(t, "numeric.Less"    , Constant(x)) => Le(t.asType.toType, x)
        case RT(t, "boolean.Not"     , List(tt))    => Not(scan(tt))
//        case RT(t, "boolean.Or"      , List(l, r))  => Or(scan(l), scan(r))
//        case RT(t, "boolean.And"     , List(l, r))  => And(scan(l), scan(r))
      }
    }

    val pred = scan(t)
    dbg(pred.toString)

    c.Expr[json.Schema[T]](
      q"""
        import json.Schema._
        import json.Validation._

        ${pred.tree}.asInstanceOf[json.Schema[$t]]
      """)
  }
}
