package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+", "=>", "|")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd", "inl", "as", "inr",
                              "case", "of", "fix", "letrec")

// ======================== PARSER ================================

  def NumberTerm (x: Int) : Term = {
    if (x == 0)
      Zero
    else
      new Succ(NumberTerm(x - 1))
  }

  /** Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = positioned(
    SimpleTerm ~ rep(SimpleTerm) ^^ {
      case firstParam ~ listParam =>
        if (listParam.length == 0)
          firstParam
        else
          listParam.foldLeft(firstParam)((x, y) => Application(x, y))
    }
    | failure("illegal start of term"))

  /** SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   */
  def SimpleTerm: Parser[Term] = positioned(
      "true" ^^^ True
    | "false" ^^^ False
    | numericLit ^^ { x => NumberTerm(x.toInt) }
    | "succ" ~> Term ^^ { case e1 => Succ(e1) }
    | "pred" ~> Term ^^ { case e1 => Pred(e1) }
    | "iszero" ~> Term ^^ { case e1 => IsZero(e1) }
    | "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ { case "if"~e1~"then"~e2~"else"~e3 => IfThenElse(e1,e2,e3) }
    | ident ^^ { case x => Variable(x) }
    | "\\" ~ ident ~ ":" ~ Type ~ "." ~ Term ^^ { case "\\" ~ param ~ ":" ~ t ~ "." ~ body => Lambda(Variable(param), t, body) }
    | "(" ~ Term ~ ")" ^^ { case "(" ~ param ~ ")" => param }
    | "let" ~ ident ~ ":" ~ Type ~ "=" ~ Term ~ "in" ~ Term ^^ { case "let" ~ n ~ ":" ~ tip ~ "=" ~ t1 ~ "in" ~ t2 => Let(n, tip, t1, t2) }
    | "{" ~ Term ~ "," ~ Term ~ "}" ^^ { case "{" ~ t1 ~ "," ~ t2 ~ "}" => Pair(t1, t2) }
    | "fst" ~> Term ^^ { case x => Fst(x) }
    | "snd" ~> Term ^^ { case x => Snd(x) }
    | "inl" ~ Term ~ "as" ~ Type ^^ { case "inl" ~ t ~ "as" ~ tip => InjectLeft(t, tip) }
    | "inr" ~ Term ~ "as" ~ Type ^^ { case "inr" ~ t ~ "as" ~ tip => InjectRight(t, tip) }
    | "case" ~ Term ~ "of" ~ "inl" ~ ident ~ "=>" ~ Term ~ "|" ~ "inr" ~ ident ~ "=>" ~ Term ^^ {
      case "case" ~ t ~ "of" ~ "inl" ~ inl_var ~ "=>" ~ inl_term ~ "|" ~ "inr" ~ inr_var ~ "=>" ~ inr_term =>
        Case(t, Variable(inl_var), inl_term, Variable(inr_var), inr_term)
    }
    | "fix" ~ Term ^^ { case "fix" ~ t => Fix(t) }
    | "letrec" ~ ident ~ ":" ~ Type ~ "=" ~ Term ~ "in" ~ Term ^^ { case "letrec" ~ n ~ ":" ~ tip ~ "=" ~ t1 ~ "in" ~ t2 => Letrec(n, tip, t1, t2) }
    | failure("illegal start of simple term")
  )

  /** Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = positioned(
    PairType ~ "->" ~ Type ^^ { case st ~ "->" ~ t => TypeFunction(st, t) }
    | PairType ^^ { case x => x }
    | failure("illegal start of type"))

  def PairType: Parser[Type] = positioned(
    SimplyType ~ "*" ~ PairType ^^ { case st ~ "*" ~ t => TypePair(st, t) }
    | SimplyType ~ "+" ~ PairType ^^ { case st ~ "+" ~ t => TypeSum(st, t) }
    | SimplyType ^^ { case x => x }
  )

  def SimplyType: Parser[Type] = positioned(
    "Bool" ^^^ TypeBool
    | "Nat" ^^^ TypeNat
    | "(" ~ Type ~ ")" ^^ { case "(" ~ t ~ ")" => t }
  )

//====================== TYPE INFERENCE ================================

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    case Succ(x) => isNumericVal(x)
    case Zero => true
    case _ => false
  }

  /** Is the given term a numeric value? */
  def isBoolVal(t: Term): Boolean = t match {
    case True => true
    case False => true
    case _ => false
  }

  /** Is the given term a value? */
  def isLambdaValue(t: Term): Boolean = t match {
    case Lambda(_, _, _) => true
    case _ => false
  }

  def isPairValue(t: Term): Boolean = t match {
    case Pair(x, y) => isValue(x) && isValue(y)
    case _ => false
  }

  def isSumValue(t: Term): Boolean = t match {
    case InjectLeft(v, tip) => isValue(v)
    case InjectRight(v, tip) => isValue(v)
    case _ => false
  }

  def isValue(t: Term): Boolean = isNumericVal(t) || isBoolVal(t) || isLambdaValue(t) || isPairValue(t) || isSumValue(t)

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]


  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False =>
      TypeBool
    case Zero =>
      TypeNat

    case Pred(x) =>
      typeof(ctx, x) match {
        case TypeNat => TypeNat
        case _ => throw new TypeError(x.pos, "[pred] expected numerical value")
     }

    case Succ(x) =>
      typeof(ctx, x) match {
        case TypeNat => TypeNat
        case _ => throw new TypeError(x.pos, "[succ] expected numerical value")
     }

    case IsZero(x) =>
      typeof(ctx, x) match {
        case TypeNat => TypeBool
        case _ => throw new TypeError(x.pos, "[iszero] expected numerical value")
    }

    case IfThenElse(e1, e2, e3) =>
      typeof(ctx, e1) match {
        case TypeBool => {
            val type2 = typeof(ctx, e2)
            val type3 = typeof(ctx, e3)
            if (type2 == type3)
              return type2
            else
              throw new TypeError(e2.pos, "[If-Then-Else] branches return different types: " + type2 + " vs " + type3)
        }
        case _ => throw new TypeError(e1.pos, "[If-Then-Else]expected boolean value")
      }

     case Lambda(param, tip, body) =>
      typeof((param.name, tip) :: ctx, body) match {
        case t => TypeFunction(tip, t)
      }
     
    case Variable(x) =>
      ctx find { e => e._1 == x } match {
        case Some((x, tip)) => tip
        case None => throw new TypeError(t.pos, "[Variable] variable " + x + " is not bounded by the current context")
      }

    case Application(left, right) =>
      val type_left = typeof(ctx, left)
      val type_right = typeof(ctx, right)

      type_left match {
        case TypeFunction(tip1, tip2) =>
          if (tip1 == type_right)
            return tip2
          throw new TypeError(t.pos, "[Application] application has conflict types: " + tip1 + " vs " + type_right)
        case _ => throw new TypeError(t.pos, "[Application] application expected as the left term")
      }

    case Pair(fst, snd) => TypePair(typeof(ctx, fst), typeof(ctx, snd))

    case Fst(x) =>
      typeof(ctx, x) match {
        case TypePair(t1, t2) => t1
        case _ => throw new TypeError(t.pos, "[Fst] expected pair type")
      }

    case Snd(x) =>
      typeof(ctx, x) match {
        case TypePair(t1, t2) => t2
        case _ => throw new TypeError(t.pos, "[Snd] expected pair type")
      }

    case Let(n, tip, t1, t2) =>
      val type_left = tip
      val type_right = typeof(ctx, t1)

      if (type_left == type_right)
          typeof((n, tip) :: ctx, t2)
      else
        throw new TypeError(t.pos, "[Let] let has conflict types: " + type_left + " vs " + type_right)

    case InjectLeft(t1, tip) =>
    {
      val type1 = typeof(ctx, t1)
      tip match {
        case TypeSum(type2, _) =>
          if (type1 == type2)
            tip
          else
            throw new TypeError(t.pos, "[InjectLeft] left type doest not match " + type1 + " vs " + type2)
        case y => throw new TypeError(t.pos, "[InjectLeft] type does not evaluate to TypeSum: " + y)
      }
    }

    case InjectRight(t1, tip) =>
    {
      val type1 = typeof(ctx, t1)
      tip match {
        case TypeSum(_, type2) =>
          if (type1 == type2)
            tip
          else
            throw new TypeError(t.pos, "[InjectRight] right type doest not match " + type1 + " vs " + type2)
        case y => throw new TypeError(t.pos, "[InjectRight] right does not evaluate to TypeSum: " + y)
      }
    }

    case Case(t0, inl_var, inl_term, inr_var, inr_term) =>
    {
      var type0 = typeof(ctx, t0)
      type0 match {
        case TypeSum(type1, type2) =>
        {
          var type_var1 = typeof((inl_var.name, type1) :: ctx, inl_term)
          var type_var2 = typeof((inr_var.name, type2) :: ctx, inr_term)
          if (type_var1 == type_var2)
            return type_var1
          else
            throw new TypeError(t.pos, "[Case] branches have different types: " + type_var1 + " vs " + type_var2)
        }
        case y => throw new TypeError(t.pos, "[Case] expected type to be TypeSum: " + type0)
      }
    }

    case Fix(x) => {
      var type0 = typeof(ctx, x)
      type0 match {
        case TypeFunction(type1, type2) => {
          if (type1 == type2)
            type1
          else
            throw new TypeError(t.pos, "[Fix] function not of type T->T: " + type1 + " vs " + type2)
        }
        case type3 =>  throw new TypeError(t.pos, "[Fix] function type expected as parameter, not: " + type3)
      }
    }

    case Letrec(n, tip, t1, t2) =>
      val type_left = tip
      val type_right = typeof((n, tip) :: ctx, t1)

      if (type_left == type_right)
          typeof((n, tip) :: ctx, t2)
      else
        throw new TypeError(t.pos, "[Letrec] let has conflict types: " + type_left + " vs " + type_right)
  }


// =============================== ONE STEP EVALUATOR =============

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString = 
      msg + "\n" +  pos.longString
  }

  /** Counter for the variables renaming operation */
  var m_counter = 0
  def count: Int = {
    m_counter = m_counter + 1
    m_counter
  }

  def alpha(t: Term): Term = t match {
    case Lambda(Variable(old_var), tip, body) =>
        val new_var = Variable(old_var + count)
        Lambda(new_var, tip, subst(body, old_var, new_var))
    case Variable(old_var) => Variable(old_var + count)
    case _ => t
  }

  def getFreevars(t: Term): Set[String] = t match {
    case Variable(v) => Set() + v
    case Lambda(Variable(v), tip, body) => getFreevars(body) - v
    case Application(t1, t2) => getFreevars(t1) ++ getFreevars(t2)
    case _ => Set()
  }

  def subst(t: Term, x: String, s: Term): Term = t match {
    case Variable(y) =>
        if (y == x)
            s
        else
            t

    case Lambda(param, tip, body) =>
        if (param.name == x)
          t
        else if (!(getFreevars(s) contains param.name))
          Lambda(param, tip, subst(body, x, s))
        else
          // Rename and then try to substitute
          subst(alpha(t), x, s)

    case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
   
    case IsZero(t1) => IsZero(subst(t1, x, s))

    case Pred(t1) => Pred(subst(t1, x, s))
    case Succ(t1) => Succ(subst(t1, x, s))
    case IfThenElse(t1, t2, t3) => IfThenElse(subst(t1, x, s), subst(t2, x, s), subst(t3, x, s))
    case Let(n, tip, t1, t2) => {
        if (n != x) 
            Let(n, tip, subst(t1, x, s), subst(t2,x,s))
        else
            Let(n, tip, subst(t1,x,s) , t2)
    }
    case Pair(t1,t2) => Pair(subst(t1, x, s), subst(t2,x,s))
    case Fst(t1) => Fst(subst(t1, x, s))
    case Snd(t1) => Snd(subst(t1, x, s))
    case InjectLeft(t1, type1) => InjectLeft(subst(t1, x, s), type1)
    case InjectRight(t1, type1) => InjectRight(subst(t1, x, s), type1)
    case Case(t1, inl_var, inl_term, inr_var, inr_term) => 
        if (x != inl_var.name && x != inr_var.name)
          Case(subst(t1, x, s), inl_var, subst(inl_term, x, s), inr_var, subst(inr_term, x, s))
        else if (x != inl_var.name)
          Case(subst(t1, x, s), inl_var, subst(inl_term, x, s), inr_var, inr_term)
        else if (x != inr_var.name)
          Case(subst(t1, x, s), inl_var, inl_term, inr_var, subst(inr_term, x, s))
        else
          Case(subst(t1, x, s), inl_var, inl_term, inr_var, inr_term)
    case Fix(t1) => Fix(subst(t1, x, s))
    case Letrec(n, tip, t1, t2) => {
        if (n != x)
            Letrec(n, tip, subst(t1, x, s), subst(t2,x,s))
        else
            Letrec(n, tip, subst(t1,x,s) , t2)
    }
    case x => x
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    case IsZero(Zero) => { True }
    case IsZero(Succ(x)) =>
      if (isNumericVal(x))
        False
      else
        IsZero(Succ(reduce(x)))
    case IsZero(x) => IsZero(reduce(x))
    
    case IfThenElse(True, e2, e3) => e2
    case IfThenElse(False, e2, e3) => e3
    case IfThenElse(e1, e2, e3) => IfThenElse(reduce(e1), e2, e3)

    case Succ(x) => {
      if (isNumericVal(x))
        throw new NoRuleApplies(t)
      else
        Succ(reduce(x))
    }

    case Pred(Zero) => { Zero }
    case Pred(Succ(x)) => {
      if (isNumericVal(x))
        x
      else
        Pred(Succ(reduce(x)))
    }
    case Pred(x) => Pred(reduce(x))

    case Application(Lambda(x, tip, t12), v2) =>
        if (isValue(v2))
          subst(t12, x.name, v2)
        else
          Application(Lambda(x, tip, t12), reduce(v2))
    case Application(t1, t2) =>
      if (isValue(t1) && isValue(t2))
        throw new NoRuleApplies(t)
      else if (isValue(t1))
        Application(t1, reduce(t2))
      else
        Application(reduce(t1), t2)

    case Let(n, tip, t1, t2) =>
        if (isValue(t1))
          subst(t1, n, t2)
        else
          Let(n, tip, reduce(t1), t2)

    case Pair(t1, t2) => {
      if (isValue(t1) && isValue(t2))
        throw new NoRuleApplies(t1)
      else if (isValue(t1))
        Pair(t1, reduce(t2))
      else
        Pair(reduce(t1), t2)
    }

    case Fst(Pair(t1, t2)) =>
    {
      if (isValue(t1) && isValue(t2))
        t1
      else
        Fst(reduce(Pair(t1, t2)))
    }

    case Fst(t) => Fst(reduce(t))

    case Snd(Pair(t1, t2)) =>
    {
      if (isValue(t1) && isValue(t2))
        t2
      else
        Snd(reduce(Pair(t1, t2)))
    }
    
    case Snd(t) => Snd(reduce(t))

    case Case(t0, inl_var, inl_term, inr_var, inr_term) =>
    {
      t0 match {
        case InjectLeft(v0, type0) => {
          if (isValue(v0))
            subst(inl_term, inl_var.name, v0)
          else
            Case(reduce(t0), inl_var, inl_term, inr_var, inr_term)
        }
        case InjectRight(v0, type0) => {
          if (isValue(v0))
            subst(inr_term, inr_var.name, v0)
          else
            Case(reduce(t0), inl_var, inl_term, inr_var, inr_term)
        }
        case _ => throw new NoRuleApplies(t)
      }
    }

    case InjectLeft(t, tip) => InjectLeft(reduce(t), tip)
    case InjectRight(t, tip) => InjectRight(reduce(t), tip)

    case Fix(t) =>
    {
      t match {
        case Lambda(x, tip, t2) => subst(t2, x.name, Fix(Lambda(x, tip, t2)))
        case y => {
          if (isValue(y))
            throw new NoRuleApplies(t)
          else
            Fix(reduce(t))
        }
      }
    }

    case Letrec(n, tip, t1, t2) => subst(t2, n, Fix(Lambda(Variable(n), tip, t1)))

    case _ => throw new NoRuleApplies(t)
  }


  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
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
        println(trees)
        try {
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => println("[Type error] " + tperror.toString) 
        }
      case e =>
        println("[Parse error] " + e)
    }
  }
}
