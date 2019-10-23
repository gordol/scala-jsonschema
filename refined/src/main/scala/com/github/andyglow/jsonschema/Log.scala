package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

private[jsonschema] trait Log {

  val c: blackbox.Context

  val dbg: String => Unit = c.info(c.enclosingPosition, _, force = true) // _ => () //

  val err: String => Nothing = c.abort(c.enclosingPosition, _)
}
