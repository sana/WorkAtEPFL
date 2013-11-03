package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Variable(name: String) extends Term {override def toString()={name} } 

case class Lambda(param: Variable, body: Term) extends Term {override def toString()={"(" + "\\" + param + "." + body + ")"} } 
case class Application(left: Term, right: Term) extends Term {override def toString()={"(" + left + " " + right + ")"} } 

