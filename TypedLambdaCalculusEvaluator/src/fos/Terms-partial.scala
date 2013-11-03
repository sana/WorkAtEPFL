package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}

case object False extends Term {
  override def toString() = "false"
}

case object Zero extends Term
{
  override def toString() = "Zero"
}

case class IsZero(t: Term) extends Term
{
  override def toString() = "(iszero " + t + ")"
}

case class IfThenElse(cond: Term, t1: Term, t2: Term) extends Term
{
  override def toString() = "if " + cond + " then " + t1 + " else " + t2
}

case class Succ(t: Term) extends Term
{
  override def toString() = "(succ " + t + ")"
}

case class Pred(t: Term) extends Term
{
  override def toString() = "(pred " + t + ")"
}

case class Variable(name: String) extends Term
{
  override def toString() = name
}

case class Lambda(param: Variable, t: Type, body: Term) extends Term
{
  override def toString() = "(" + "\\" + param + ":" + t + "." + body + ")"
}

case class Application(left: Term, right: Term) extends Term
{
  override def toString() = "(" + left + " " + right + ")"
}

// Pairs and Let
case class Let(n: String, tip: Type, t1: Term, t2: Term) extends Term
{
  override def toString() = "(" + "let " + n + ":" + tip + " = " + t1 + " in " + t2 + ")"
}

case class Pair(fst: Term, snd: Term) extends Term
{
  override def toString() = "{" + fst + "," + snd + "}"
}

case class Fst(t: Term) extends Term
{
  override def toString() = "fst(" + t + ")"
}

case class Snd(t: Term) extends Term
{
  override def toString() = "snd(" + t + ")"
}



/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type
{
  override def toString() = "Bool"
}

case object TypeNat extends Type
{
  override def toString() = "Nat"
}

case class TypePair(u: Type, v: Type) extends Type
{
  override def toString() = "(" + u + "*" + v + ")"
}

case class TypeFunction(u: Type, v: Type) extends Type
{
  override def toString() = "(" + u + "->" + v + ")"
}

