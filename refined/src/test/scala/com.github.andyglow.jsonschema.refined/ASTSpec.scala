package com.github.andyglow.jsonschema.refined

import org.scalatest.WordSpec

import scala.reflect.macros.blackbox

class ASTSpec extends WordSpec {

  "" should {
    "" in {

      val x = new AST with Math with Log {
        override val c: blackbox.Context = _
      }
    }
  }
}
