package com.github.andyglow.jsonschema.refined

import eu.timepit.refined.api.Refined

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class RefinedMacro(val c: blackbox.Context) extends Logic {
  import c.universe._

  def impl[A, B](implicit a: c.WeakTypeTag[A], b: c.WeakTypeTag[B]): c.Expr[json.Schema[Refined[A, B]]] = {
    val t = typeOf[eu.timepit.refined.api.Refined[_, _]]
    val tt = appliedType(t.typeConstructor, List(a.tpe, b.tpe))

    dbg("\n---------------\n"+showRaw(tt)+"\n---------------")

    val tree = gen(tt)

    dbg(showCode(tree))

    c.Expr[json.Schema[Refined[A, B]]](
      q"""
        import json.Schema._
        import `string`._
        import Format._
        import json.Validation._

        $tree.asInstanceOf[json.Schema[$tt]]
      """)
  }

  def implUnsafe[T](implicit t: c.WeakTypeTag[T]): c.Expr[json.Schema[T]] = {
    import c.universe._
    val tt = t.tpe.dealias

    dbg("\n---------------\n"+showRaw(tt)+"\n---------------")

    val tree = gen(tt)

    dbg(showCode(tree))

    c.Expr[json.Schema[T]](
      q"""
        import json.Schema._
        import `string`._
        import Format._
        import json.Validation._

        $tree.asInstanceOf[json.Schema[$tt]]
      """)
  }
}

object RefinedMacro {

  def refined[T]: json.Schema[T] = macro RefinedMacro.implUnsafe[T]
}
