package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */

// Class for values : numbers or booleans
case class Value(hasnumber: Boolean, number: Int, bool: Boolean) 

case class NoRuleApplies(t: Term) extends Exception(t.toString)


object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit
  var stuck_term : Term = null
  var result : Term = null

  /** Expr ::= 'true'
      | 'false'
      | 'if' Expr 'then' Expr 'else' Expr
      | '0'
      | 'succ' Expr
      | 'pred' Expr
      | 'iszero' Expr
   */
  def Expr: Parser[Term] = (
       "true" ^^ { case _ => True(0) }
      | "false" ^^ { case _ => False(0) }
      | "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ^^ { case "if"~e1~"then"~e2~"else"~e3 => IfThenElse(e1,e2,e3) }
      | "succ" ~> Expr ^^ { case e1 => Succ(e1) }
      | "pred" ~> Expr ^^ { case e1 => Pred(e1) }
      | numericLit ^^ { x => TreeFromNumber(x.toInt) }
      | "iszero" ~> Expr ^^ { case e1 => IsZero(e1) }
      | failure("illegal start of expression")
    )

  def IsNumericalValue(x: Term) : Boolean = {
    x match {
      case Succ(y) => IsNumericalValue(y)
      case Zero(t) => true
      case _ => false
    }
  }

	def TreeFromNumber (x: Int) : Term = {
		if (x == 0) 
			new Zero(0)
		else
			new Succ(TreeFromNumber(x - 1)) 
	}

    def eval(tree: Term): Value = { Evaluator(tree) }

    /* eval */
	def Evaluator (tree : Term) : Value = {
		tree match {
			case IsZero(t) => Value(false, 0, if(Evaluator(t).number == 0) true else false)
			case IfThenElse(e1, e2, e3) => if(Evaluator(e1).bool) Evaluator(e2) else Evaluator(e3)
			case Succ(t) => Value(true, Evaluator(t).number + 1 , true)
			case Pred(t) => Value(true, if(Evaluator(t).number == 0) 0 else (Evaluator(t).number - 1), true)
			case True(t) => Value(false, 0, true)
			case False(t) => Value(false, 0, false)
			case Zero(t) => Value(true, 0 , true)
		} 
	}

    def reduce(tree: Term): Term = { OneStepMinimized(tree) }

    /* reduce */
	def OneStepMinimized (tree : Term) : Term = {
		tree match {
            /** IsZero expects a numeric value */
			case IsZero(Zero(t)) => { True(0)}
			case IsZero(Succ(t)) =>
            {
              if (IsNumericalValue (t))
                False(0)
              else
                IsZero(Succ(OneStepMinimized(t)))
            }
            case IsZero(t) => IsZero(OneStepMinimized(t))

            /** If Then Else */
			case IfThenElse(True(t), e2, e3) => e2
			case IfThenElse(False(t), e2, e3) => e3
			case IfThenElse(e1, e2, e3) => IfThenElse(OneStepMinimized(e1), e2, e3) 

			case Succ(t) =>
            {
              if (IsNumericalValue(t))
                throw new NoRuleApplies(Succ(t));
              else
                Succ(OneStepMinimized(t))
            }

            case Pred(Zero(t)) => Zero(t)

			case Pred(Succ(t)) =>
            {
              if (IsNumericalValue (t))
                t
              else
                Pred(Succ(OneStepMinimized(t)))
            }

			case Pred(t) => Pred(OneStepMinimized(t))

			case t => throw new NoRuleApplies(t)
		} 
	}

  def PrintTree (tree : Term) : Unit = {
		tree match {
			case IsZero(t) => {print("IsZero("); PrintTree(t);print(")")}
			case IfThenElse(e1, e2, e3) => {print("If("); PrintTree(e1);print(",");PrintTree(e2);print(",");PrintTree(e3);print(")")}
			case Succ(t) => {print("Succ("); PrintTree(t);print(")")}
			case Pred(t) => {print("Pred("); PrintTree(t);print(")")}
			case True(t) => {print("True")}
			case False(t) => {print("False")}
			case Zero(t) => {print("Zero")}
		} 
  }

  def main(args: Array[String]): Unit = {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val tokens = new lexical.Scanner(args(0))
    var loop = true;

    phrase(Expr)(tokens) match
    {
		case Success(trees, _) => 
		{
          var new_tree = trees
          PrintTree(new_tree)
          println("")

          while(loop)
          {
            try
            {
              new_tree = OneStepMinimized(new_tree)
              PrintTree(new_tree)
              println("")
            } catch {
              case NoRuleApplies(_) => loop = false
            }
          }
		}		
      case e =>
        println("Failure at parsing " + e)
    }    
  }
}

