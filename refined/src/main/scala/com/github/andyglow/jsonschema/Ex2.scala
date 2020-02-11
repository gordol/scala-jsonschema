package com.github.andyglow.jsonschema

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.collection._
import RefinedSupport._
import com.github.andyglow.json.JsonFormatter
import json.schema.Version.Draft04

object Ex2 {
//  implicit val lpr: json.Schema[Long Refined Positive] = refinedJsonSchema[Long Refined Positive]("positive")
//  implicit val sz: json.Schema[String Refined MinSize[W.`7`.T]] = refinedJsonSchema[String Refined MinSize[W.`7`.T]]("standardName")

  def main(args: Array[String]): Unit = {
    val s = json.Json.schema[Foo]
    println(JsonFormatter.format(AsValue.schema(s, Draft04())))

//    println(json.Json.sig[Long Refined Positive])
  }
}

case class Foo(id: Long Refined Positive, name: String Refined MinSize[W.`7`.T])