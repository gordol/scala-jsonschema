package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

private[jsonschema] trait Extractors { this: Log with AST =>
  import Pred._

  val c: blackbox.Context
  import c.universe._

  private lazy val `Refined` = typeOf[eu.timepit.refined.api.Refined[_, _]].typeSymbol

  private lazy val List(
    // string
    sIPv4,
    sIPv6,
    // numeric
    sPositive,
    sNonPositive,
    sNegative,
    sNonNegative,
    sGreater,
    sGreaterEqual,
    sLess,
    sLessEqual,
    // boolean
    sNot,
    sAnd,
    sOr) = {

    prepare(
      typeOf[eu.timepit.refined.string.IPv4],
      typeOf[eu.timepit.refined.string.IPv6],
      typeOf[eu.timepit.refined.numeric.Positive],
      typeOf[eu.timepit.refined.numeric.NonPositive],
      typeOf[eu.timepit.refined.numeric.Negative],
      typeOf[eu.timepit.refined.numeric.NonNegative],
      typeOf[eu.timepit.refined.numeric.Greater[_]],
      typeOf[eu.timepit.refined.numeric.GreaterEqual[_]],
      typeOf[eu.timepit.refined.numeric.Less[_]],
      typeOf[eu.timepit.refined.numeric.LessEqual[_]],
      typeOf[eu.timepit.refined.boolean.Not[_]],
      typeOf[eu.timepit.refined.boolean.And[_, _]],
      typeOf[eu.timepit.refined.boolean.Or[_, _]])
  }


  def prepare(x: Type, xs: Type*): List[Symbol] = {
    val tuples = (x +: xs.toList) map {
      case t @ TypeRef(_, s, _) => (t, s)
      case ExistentialType(_, t@ TypeRef(_, s, _)) => (t, s)
      case t                    => (t, t.typeSymbol)
    }

    val sb = new StringBuilder
    tuples foreach { case (t, s) =>
      sb.append("T: ").append(showRaw(t)).append(" ;")
      sb.append("S: ").append(showRaw(s)).append("\n")
    }
    dbg("\nPREPARED\n" + sb.toString)

    tuples map { case (_, s) => s }
  }

  object refined {

    object R {

      def unapply(t: Type): Option[(Symbol, TypeRef)] =
        t match {
          case TypeRef(_, `Refined`, List(TypeRef(_, t, _), p: TypeRef)) => Some((t, p))
          case _                                                               => None
        }
    }

    object P {

      def unapply(tuple: (Type, TypeRef)): Option[Pred] = {
        val (t, p) = tuple
        dbg(s"P.unapply: t=${showRaw(t)}, p=${showRaw(p)}")

        object Ex {
          def unapply(p: Type): Option[Pred] = {
            dbg(s"Ex.unapply: p=${showRaw(p)}")

            val v = p match {
              // string
              case TypeRef(_, `sIPv4`         , _)                                => Some(IPv4(t))
              case TypeRef(_, `sIPv6`         , _)                                => Some(IPv6(t))
              // numeric
              case TypeRef(_, `sPositive`     , _)                                => Some(Pos(t))
              case TypeRef(_, `sNonPositive`  , _)                                => Some(Not(Pos(t)))
              case TypeRef(_, `sNegative`     , _)                                => Some(Neg(t))
              case TypeRef(_, `sNonNegative`  , _)                                => Some(Not(Neg(t)))
              case TypeRef(_, `sGreater`      , List(ConstantType(Constant(v))))  => Some(Ge(t, v))
              case TypeRef(_, `sGreaterEqual` , List(ConstantType(Constant(v))))  => Some(Ge(t, v, inclusive = true))
              case TypeRef(_, `sLess`         , List(ConstantType(Constant(v))))  => Some(Le(t, v))
              case TypeRef(_, `sLessEqual`    , List(ConstantType(Constant(v))))  => Some(Le(t, v, inclusive = true))
              // boolean
              case TypeRef(_, `sNot`          , List(Ex(pp)))                     => Some(Not(pp))
              case TypeRef(_, `sAnd`          , List(Ex(ll), Ex(rr)))             => Some(And(ll, rr))
              case TypeRef(_, `sOr`           , List(Ex(ll), Ex(rr)))             => Some(Or(ll, rr))
              case _                                                              => None
            }
            dbg(s"Ex.unapply: result ${showRaw(v)}")

            v
          }
        }

        Ex unapply p
      }
    }
  }
}
