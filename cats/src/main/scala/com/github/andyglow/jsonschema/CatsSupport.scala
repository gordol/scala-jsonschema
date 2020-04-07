package com.github.andyglow.jsonschema

import cats.data._
import json.Schema
import json.Schema._
import json.Validation._


object CatsSupport {
  import ValidationBound.mk

  implicit def nelVB[X]: ValidationBound[NonEmptyList[X], Iterable[_]] = mk[NonEmptyList[X], Iterable[_]]
  implicit def nelSchema[T](implicit ss: Schema[T]): Schema[NonEmptyList[T]] = `array`[T, NonEmptyList](ss).withValidation(`minItems` := 1)

  implicit def nesVB[X]: ValidationBound[NonEmptySet[X], Iterable[_]] = mk[NonEmptySet[X], Iterable[_]]
  implicit def nesSchema[T](implicit ss: Schema[T]): Schema[NonEmptySet[T]] = `array`[T, NonEmptySet](ss).withValidation(`minItems` := 1)

  implicit def necVB[X]: ValidationBound[NonEmptyChain[X], Iterable[_]] = mk[NonEmptyChain[X], Iterable[_]]
  implicit def necSchema[T](implicit ss: Schema[T]): Schema[NonEmptyChain[T]] = `array`[T, NonEmptyChain](ss).withValidation(`minItems` := 1)

  implicit def nemStrVB[X]: ValidationBound[NonEmptyMap[String, X], Map[String, _]] = mk[NonEmptyMap[String, X], Map[String, _]]
  implicit def nemStrSchema[T](implicit ss: Schema[T]): Schema[NonEmptyMap[String, T]] = `string-map`[T, NonEmptyMap](ss).withValidation(`minProperties` := 1)
}
