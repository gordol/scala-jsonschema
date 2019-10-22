package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric._
import json.schema.Version.Draft04

object Example {

  def main(args: Array[String]): Unit = {
    {
      type P = Int Refined Positive
      val s0 = RefinedMacro.refined[P]
      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
    }

    {
      type P = Long Refined Positive
      val s0 = RefinedMacro.refined[P]
      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
    }

    {
      type P = Double Refined Positive
      val s0 = RefinedMacro.refined[P]
      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
    }

    {
      type N = Int Refined Negative
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Int Refined Greater[W.`6`.T]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Int Refined Greater[W.`0.6`.T]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Int Refined Less[W.`0.6`.T]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

//    {
//      type N = Int Refined Not[Negative]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Int Refined Not[Positive]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
  }
}
