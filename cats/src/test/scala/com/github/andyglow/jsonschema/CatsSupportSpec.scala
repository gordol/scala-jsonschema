package com.github.andyglow.jsonschema

import cats.data._
import com.github.andyglow.json.ToValue
import json.Schema
import json.Schema.ValidationBound.mk
import json.Schema.`object`._
import json.Schema._
import json.Validation.`minItems`
import org.scalatest._
import org.scalatest.Matchers._
import scala.languageFeature.implicitConversions


class CatsSupportSpec extends WordSpec {
  import CatsSupportSpec._

  "CatsSupportSpec" when {

    "NonEmptyList" should {

      "be exposed as object" in {
        nelEventSchema shouldBe `object`(
          Field("id", `string`(None, None)),
          Field("arr", ref("cats.data.NonEmptyList", `array`[String, NonEmptyList](`string`(None, None)))))
      }
    }

//    "NonEmptySet" should {
//
//      "be exposed as object" in {
//        nesEventSchema shouldBe `object`(
//          Field("id", `string`(None, None)),
//          Field("arr", ref("cats.data.NonEmptySet", `array`[String, NonEmptyList](`string`(None, None)))))
//      }
//    }
  }
}

object CatsSupportSpec {
  import Schema._
//  import CatsSupport._

//  implicit def stringNelSchema: Schema[NonEmptyList[String]] = {
//    implicit def nelVB[X]: ValidationBound[NonEmptyList[X], Iterable[_]] = mk[NonEmptyList[X], Iterable[_]]
//    `array`[String, NonEmptyList](implicitly[Schema[String]]).withValidation(`minItems` := 1)
//  }

  implicit def nelSchema[T](implicit ss: Schema.Predefined[T]): Schema[NonEmptyList[T]] = {
    implicit def nelVB[X]: ValidationBound[NonEmptyList[X], Iterable[_]] = mk[NonEmptyList[X], Iterable[_]]
    `array`[T, NonEmptyList](ss.schema).withValidation(`minItems` := 1)
  }

  case class NonEmptyListEvent(id: String, arr: NonEmptyList[String])
  lazy val nelEventSchema: Schema[NonEmptyListEvent] = json.Json.schema[NonEmptyListEvent]

//  case class NonEmptySetEvent(id: String, arr: NonEmptySet[String])
//  lazy val nesEventSchema: Schema[NonEmptySetEvent] = json.Json.schema[NonEmptySetEvent]
//
//  case class NonEmptyChainEvent(id: String, arr: NonEmptyChain[String])
//  lazy val necEventSchema: Schema[NonEmptyChainEvent] = json.Json.schema[NonEmptyChainEvent]
//
//  case class NonEmptyMapEvent(id: String, data: NonEmptyMap[String, String])
//  lazy val nemEventSchema: Schema[NonEmptyMapEvent] = json.Json.schema[NonEmptyMapEvent]
}