package com.github.andyglow.jsonschema.refined

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import eu.timepit.refined.boolean._
import json.Json.schema
import json.Schema._
import json.Validation._
import org.scalatest.Matchers._
import org.scalatest._

import com.github.andyglow.jsonschema.RefinedSupport._


class RefinedBooleanSpec extends FunSuite {

  test("not") {
//      schema[Refined[Int, Not[GreaterEqual[W.`2`.T]]]] shouldBe `number`[Int].withValidation(`exclusiveMinimum` := 2)
//      schema[Refined[Int, Not[Divisible[W.`2`.T]]]] shouldBe json.Schema.`not`[Int](`number`[Int].withValidation(`multipleOf` := 2))
  }

  test("not/not") {
    schema[Refined[Int, Not[Not[Divisible[W.`2`.T]]]]] shouldBe `number`[Int].withValidation(`multipleOf` := 2)
  }
}
