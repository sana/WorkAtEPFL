/** Foundations of Software, Homework #1 */
package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class IsZero(t: Term) extends Term
case class IfThenElse(e1: Term, e2: Term, e3: Term ) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class True(t: Int) extends Term { override def toString() = { "True" } }
case class False(t: Int) extends Term { override def toString() = { "False" } }
case class Zero(t: Int) extends Term { override def toString() = { "Zero" } }
