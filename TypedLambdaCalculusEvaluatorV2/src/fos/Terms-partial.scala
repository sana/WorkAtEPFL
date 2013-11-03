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

case class InjectLeft(t: Term, tip: Type) extends Term
{
  override def toString() = "(inl " + t + " as " + tip + ")"
}

case class InjectRight(t: Term, tip: Type) extends Term
{
  override def toString() = "(inr " + t + " as " + tip + ")"
}

case class Case(t: Term, inl_var: Variable, inl_term: Term, inr_var: Variable, inr_term: Term) extends Term
{
  override def toString() = "(case " + t + " of inl " + inl_var + " => " + inl_term + " | inr " + inr_var + " => " + inr_term + ")"
}

case class Fix(t: Term) extends Term
{
  override def toString() = "(fix " + t + ")"
}

case class Letrec(x: String, tip1: Type, t1: Term, t2: Term) extends Term
{
  override def toString() = "(letrec " + x + ": " + tip1 + " = " + t1 + " in " + t2 + ")"
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

case class TypeSum(u: Type, v: Type) extends Type
{
  override def toString() = "(" + u + "+" + v + ")"
}

