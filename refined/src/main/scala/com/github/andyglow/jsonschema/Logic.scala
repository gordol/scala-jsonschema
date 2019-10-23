package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

private[jsonschema] trait Logic extends Extractors with Log with AST with Math {
  import refined._

  val c: blackbox.Context
  import c.universe._

  def gen(t: Type): Tree = {

    val pred = t match {
      case R(t, p) =>
        (t.asType.toType, p) match {
          case P(pp) => pp
          case _     => err(s"Can't infer Predicate out of ${showRaw(p)}")
        }
      case _ => err(s"Can't infer Refined out of ${showRaw(t)}")
    }

    dbg(pred.toString + " ----> " + pred.norm.toString)

    pred.tree
  }
}
