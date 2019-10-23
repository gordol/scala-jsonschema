package com.github.andyglow.jsonschema

import scala.reflect.macros.blackbox

private[jsonschema] trait AST { this: Log with Math =>
  import c.universe._

  sealed trait Pred {
    def t: Type
    def tree: Tree = norm.tree
    def norm: Pred = this
  }

  object Pred {

    // ------
    // STRING
    // ------
    case class IPv4(t: Type) extends Pred {
      override def tree = q"`string`[$t](Some(`ipv4`), None)"
    }

    case class IPv6(t: Type) extends Pred {
      override def tree = q"`string`[$t](Some(`ipv6`), None)"
    }

    // -------
    // NUMERIC
    // -------

    def Pos(t: Type) = Ge(t, 0)
    def NonPos(t: Type) = Le(t, 0, inclusive = true)
    def Neg(t: Type) = Le(t, 0)
    def NonNeg(t: Type) = Ge(t, 0, inclusive = true)

    case class Ge(t: Type, v: Any, inclusive: Boolean = false) extends Pred {
      override def tree = {
        val vv = v.asInstanceOf[Number].doubleValue()
        inclusive match {
          case true  => q"`number`[$t]() withValidation ( `minimum` := $vv )"
          case false => q"`number`[$t]() withValidation ( `exclusiveMinimum` := $vv )"
        }
      }

      def min(o: Ge): Ge = {
        require(t =:= o.t)

        Ge(t, math.min(v, o.v), inclusive || o.inclusive)
      }

      def max(o: Ge): Ge = {
        require(t =:= o.t)

        Ge(t, math.max(v, o.v), inclusive || o.inclusive)
      }
    }

    case class Le(t: Type, v: Any, inclusive: Boolean = false) extends Pred {
      override def tree = {
        val vv = v.asInstanceOf[Number].doubleValue()
        inclusive match {
          case true  => q"`number`[$t]() withValidation ( `maximum` := $vv )"
          case false => q"`number`[$t]() withValidation ( `exclusiveMaximum` := $vv )"
        }
      }

      def max(o: Le): Le = {
        require(t =:= o.t)

        Le(t, math.max(v, o.v), inclusive || o.inclusive)
      }

      def min(o: Le): Le = {
        require(t =:= o.t)

        Le(t, math.min(v, o.v), inclusive || o.inclusive)
      }
    }

    // -------
    // BOOLEAN
    // -------

    case class Not(p: Pred) extends Pred {
      def t = p.t
      override def tree = q"`not`[$t](${p.tree})"
      override def norm: Pred = {
        def compile(p: Pred): Pred = p match {
          case Ge(t, v, i) => Le(t, v, !i)
          case Le(t, v, i) => Ge(t, v, !i)
          case p: Not => compile(p.p)
          case p => Not(p) // there is no way to simplify it better
        }

        compile(p)
      }
    }

    case class OneOf(t: Type, preds: ::[Pred]) extends Pred {
      override def tree = q"`oneof`[$t](Set(..${preds map { _.tree } }))"
      override def norm: Pred = {
        val ppreds = preds.flatMap {
          case OneOf(tt, ppreds) => require(t =:= tt); ppreds
          case p                 => Some(p)
        }

        OneOf(t, ::(ppreds.head, ppreds.tail))
      }
    }

    case class Or(l: Pred, r: Pred) extends Pred {
      require(l.t =:= r.t)
      def t = l.t
      override def norm: Pred = {

        def compile(l: Pred, r: Pred): Pred = (l, r) match {
          case (l: Ge, r: Ge)   => l min r
          case (l: Le, r: Le)   => l max r
          case (l, r) => OneOf(t, ::(l, r :: Nil))
        }

        compile(l, r)
      }
    }

    case class AllOf(t: Type, preds: ::[Pred]) extends Pred {
      override def tree = q"`allof`[$t](Set(..${preds map { _.tree } }))"
      override def norm: Pred = {
        val ppreds = preds.flatMap {
          case AllOf(tt, ppreds) => require(t =:= tt); ppreds
          case p                 => Some(p)
        }

        AllOf(t, ::(ppreds.head, ppreds.tail))
      }
    }

    case class And(l: Pred, r: Pred) extends Pred {
      require(l.t =:= r.t)
      def t = l.t
      override def norm: Pred = {

        def compile(l: Pred, r: Pred): Pred = (l, r) match {
          case (l: Ge, r: Ge)   => l max r
          case (l: Le, r: Le)   => l min r
          case (l, r) => AllOf(t, ::(l, r :: Nil))
        }

        compile(l, r)
      }
    }
  }
}
