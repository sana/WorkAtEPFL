package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

/** This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /** Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = (
    AbsOrVar ~ rep(AbsOrVar) ^^
    {
      case firstParam ~ listParam =>
        if (listParam.length == 0)
          firstParam
        else
          listParam.foldLeft(firstParam)((x, y) => Application(x, y))
    }
  )

  def AbsOrVar: Parser[Term] = (
    ident ^^ { case x => Variable(x) }
    | "\\" ~ ident ~ "." ~ Term ^^ { case "\\" ~ param ~ "." ~ body => Lambda(Variable(param), body) }
    | "(" ~ Term ~ ")" ^^ { case "(" ~ param ~ ")" => param }
    | failure("illegal start of term")
  )

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Counter for the variables renaming operation */
  var m_counter = 0
  def count: Int = {
    m_counter = m_counter + 1
    m_counter
  }

  /** Alpha-conversion for consistent renaming */
  def alpha(t: Term): Term = t match {
    case Lambda(Variable(old_var), body) =>
        val new_var = Variable(old_var + count)
        Lambda(new_var, subst(body, old_var, new_var))
	case Variable(old_var) => Variable(old_var + count)
    case _ => t
  }

  /**
    FV(x)         =   {x}
    FV(x.t1)      =   FV(t1) \ {x}
    FV(t1 t2)     =   FV(t1) and FV(t2)
  */
  def getFreevars(t: Term): Set[String] = t match {
    case Variable(v) => Set() + v
    case Lambda(Variable(v), body) => getFreevars(body) - v
    case Application(t1, t2) => getFreevars(t1) ++ getFreevars(t2)
    case _ => Set()
  }

  /**
    [x -- s] x           =   s
    [x -- s] y           =   y   if y != x
    [x -- s] (y. t1)     =   y . t1     if y = x
                         =   y . [x -- s] t1  if y != x and y not in FV(s)
						 = subst(alpha(y.t1)) if y !x si y in FV(s)
    [x -- s] (t1 t2)     =   ([x -- s] t1  [x -- s] t2)
  */
  def subst(t: Term, x: String, s: Term): Term = t match {
    case Variable(y) =>
        if (y == x)
            s
        else
            t

    case Lambda(param, body) =>
        if (param.name == x)
          t
        else if (!(getFreevars(s) contains param.name))
          Lambda(param, subst(body, x, s))
        else
          // Rename and then try to substitute
          subst(alpha(t), x, s)

    case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))

    case x => x
  }

  def isValue(t: Term): Boolean = t match {
    case Lambda(_, _) => true
    case _ => false
  }


  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case Application(Lambda(x, t12), v2) => subst(t12, x.name, v2)
	case Lambda(x, t1) => Lambda(x, reduceNormalOrder(t1))

	case Application(t1, t2) =>
		try {
			Application(reduceNormalOrder(t1), t2)
		} catch {
		  case NoRuleApplies(_) =>
		    Application(t1, reduceNormalOrder(t2))
		}

    case _ => throw new NoRuleApplies(t)
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    case Application(Lambda(x, t12), v2) =>
        if (isValue(v2))
          subst(t12, x.name, v2)
        else
          Application(Lambda(x, t12), reduceCallByValue(v2))
    case Application(t1, t2) => Application(reduceCallByValue(t1), t2)
    case t => throw new NoRuleApplies(t)
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] = 
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }


  def main(args: Array[String]): Unit = {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val tokens = new lexical.Scanner(args(0))

    phrase(Term)(tokens) match {
      case Success(trees, _) =>
        println("Parsed tree")
        println(trees)

        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)
        
      case e =>
        println(e)
    }    
  }
}
