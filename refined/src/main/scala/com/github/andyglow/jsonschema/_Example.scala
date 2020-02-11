package com.github.andyglow.jsonschema

import com.github.andyglow.json.JsonFormatter
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
import eu.timepit.refined.generic._
import eu.timepit.refined.collection._
import eu.timepit.refined.string._
import json.schema.Version.Draft04
import refined._

object _Example {

  def main(args: Array[String]): Unit = {
//    {
//      type P = Int Refined Positive
//      val s0 = RefinedMacro.refined[P]
//      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
//    }
//
//    {
//      type P = Long Refined Positive
//      val s0 = RefinedMacro.refined[P]
//      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
//    }
//
//    {
//      type P = Double Refined Positive
//      val s0 = RefinedMacro.refined[P]
//      println(JsonFormatter.format(AsValue.schema(s0, Draft04())))
//    }
//
//    {
//      type N = Int Refined Negative
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Int Refined NonPositive
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Int Refined Greater[W.`6`.T]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Int Refined Greater[W.`0.6`.T]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Int Refined Less[W.`0.6`.T]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
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
//
//    {
//      type N = Int Refined Not[Not[Positive]]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }

    // shouldn't compile
//    {
//      type N = Refined[Int, Positive And Negative]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }

//    {
//      type N = Refined[Int, Positive And Greater[W.`7`.T]]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Refined[Int, Negative And Less[W.`7`.T]]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Double Refined Divisible[W.`23.6`.T]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }

    // ----------------
    // string
    // ----------------

//    {
//      type N = String Refined IPv4
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = String Refined IPv6
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//
//    {
//      type N = Refined[String, MatchesRegex[W.`".*"`.T]]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }

//    {
//      type N = Refined[String, Not[IPv6]]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }
//    {
//      type N = Refined[String, IPv6 Or IPv4]
//      val s1 = RefinedMacro.refined[N]
//      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
//    }

    // -----------
    // collections
    // -----------
    {
      type N = Refined[List[String], MinSize[W.`5`.T]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Refined[String, MinSize[W.`5`.T]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Refined[List[String], MaxSize[W.`5`.T]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Refined[String, MaxSize[W.`5`.T]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Refined[String, Size[W.`5`.T]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }

    {
      type N = Refined[String, Size[Not[Less[W.`5`.T]]]]
      val s1 = RefinedMacro.refined[N]
      println(JsonFormatter.format(AsValue.schema(s1, Draft04())))
    }
  }
}
