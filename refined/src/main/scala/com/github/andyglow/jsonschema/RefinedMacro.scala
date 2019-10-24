package com.github.andyglow.jsonschema

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class RefinedMacro(val c: blackbox.Context) extends Logic {

  def impl[T: c.WeakTypeTag]: c.Expr[json.Schema[T]] = {
    import c.universe._
    val tt = weakTypeTag[T].tpe.dealias

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

  def refined[T]: json.Schema[T] = macro RefinedMacro.impl[T]
}
