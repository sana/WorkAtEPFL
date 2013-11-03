/**
 * @author Laurentiu Dascalu
 */
import scala.collection.immutable.Set
import scala.collection.immutable.List

object QBF {

  abstract class Formula
  case class And(lhs: Formula, rhs: Formula) extends Formula {
    override def toString = "(" + lhs.toString + " & " + rhs.toString + ")"
  }
  case class Not(f:Formula) extends Formula {
    override def toString = "!" + f.toString
  }
  case class Or(lhs: Formula, rhs: Formula) extends Formula {
    override def toString = "(" + lhs.toString + " | " + rhs.toString + ")"
  }
  case class Implies(lhs: Formula, rhs: Formula) extends Formula {
    override def toString = "(" + lhs.toString + " --> " + rhs.toString + ")"
  }
  case class Variable(name:String) extends Formula {
    override def toString = name
  }
  case object False extends Formula  {
    override def toString = "false"
  }
  case object True extends Formula {
    override def toString = "true"
  }
  case class Exists(l:Variable, f: Formula) extends Formula {
    override def toString = "(EX " + l.toString + " . " + f.toString + ")"  
  }
  case class Forall(l:Variable, f: Formula) extends Formula {
    override def toString = "(ALL " + l.toString + " . " + f.toString + ")"  
  }

  /* Applies as much transformations to truth values as possible */
  def simplify(f: Formula) : Formula = {
    f match {
      /* And */
      case And(False, rhs) => False
      case And(lhs, rhs) => simplify(lhs) match {
    	  case False => False
    	  case _ => simplify(rhs)
      }
      
      /* Or */
      case Or(True, rhs) => True
      case Or(lhs, rhs) => simplify(lhs) match {
    	  case True => True
    	  case _ => simplify(rhs)
      }
      
      /* Not */
      case Not(f1) => simplify(f1) match {
    	  case True => False
    	  case False => True
    	  case x => Not(x)
      }
      
      /* Implication */
      case Implies(lhs, rhs) => simplify(lhs) match {
    	  case False => True
    	  case True => simplify(rhs)
    	  case _ => Implies(lhs, rhs)
      }
      
      /* No more simplifications */
      case x => x
    }
  }

  /* Substitutes a formula for a literal in a quantifier-free formula */
  def subst(f: Formula, l: Variable, replacement: Formula) : Formula = {
    f match {
    	case Variable(x) =>
    		if (x == l.name)
    			replacement
    		else
    			f
    	case And(lhs, rhs) =>
    		And(subst(lhs, l, replacement), subst(rhs, l, replacement))
    	case Or(lhs, rhs) =>
    		Or(subst(lhs, l, replacement), subst(rhs, l, replacement))
    	case Not(f1) => Not(subst(f1, l, replacement))
    	case Implies(lhs, rhs) => Implies(subst(lhs, l, replacement), subst(rhs, l, replacement))
    	
    	/* This does not make sense, as we know that the formula is quantifier-free
    	case Exists(l1, f1) =>
    		//   If we have the case subst(EX y . !y, y, False)
    		// then we return (EX y . !y)
    		//
    		if (l.name == l1.name)
    			f
    		//   Else we have the case subst(EX y . y | x, True)
    		// then we return (EX y . y | True)
    		//
    		else
    			Exists(l1, subst(f1, l, replacement))
    			
    	case Forall(l1, f1) =>
    		//   If we have the case subst(ALL y . !y, y, False)
    		// then we return (ALL y . !y)
    		//
    		if (l.name == l1.name)
    			f
    		//   Else we have the case subst(ALL y . y | x, True)
    		// then we return (ALL y . y | True)
    		//
    		else
    			Forall(l1, subst(f1, l, replacement))
    	*/
    	
    	case True => True
    	case False => False
    }
  }

  /* Takes a formula f and returns an equivalent formula that doesn't contain any quantifier */
  def elimQuantifiers(f: Formula) : Formula = {
    f match {
    	case Exists(l1, f1) => Or(subst(elimQuantifiers(f1), l1, True),
    			subst(elimQuantifiers(f1), l1, False))
    	case Forall(l1, f1) => And(subst(elimQuantifiers(f1), l1, True),
    			subst(elimQuantifiers(f1), l1, False))
    	case And(lhs, rhs) => And(elimQuantifiers(lhs), elimQuantifiers(rhs))
    	case Or(lhs, rhs) => Or(elimQuantifiers(lhs), elimQuantifiers(rhs))
    	case Not(f1) => Not(elimQuantifiers(f1))
    	case Implies(lhs, rhs) => Implies(elimQuantifiers(lhs), elimQuantifiers(rhs))
    	case x => x
    }
  }

  /* freeVars returns the set of free variables contained in its first argument */
  def freeVars(f: Formula, boundVars: Set[Variable]) : Set[Variable] = {
    f match {
    	case Variable(name) => {
    		if (boundVars contains Variable(name))
    			Set()
    		else
    			Set() + Variable(name)
    	}
    	case And(lhs, rhs) => freeVars(lhs, boundVars) ++ freeVars(rhs, boundVars)
    	case Or(lhs, rhs) => freeVars(lhs, boundVars) ++ freeVars(rhs, boundVars)
    	case Not(f1) => freeVars(f1, boundVars)
    	case Implies(lhs, rhs) => freeVars(lhs, boundVars) ++ freeVars(rhs, boundVars)
    	case Exists(l1, f1) => freeVars(f1, boundVars + l1)
    	case Forall(l1, f1) => freeVars(f1, boundVars + l1)
    	case True => Set()
    	case False => Set()
    }
  }

  /* With the right q, "close" may universally or existentially close f */
  def close(f: Formula, q: (Variable, Formula) => Formula) : Formula = freeVars(f, Set()).foldLeft(f)((x:Formula, y:Variable) => q(y, x))

  def univClose(f: Formula) : Formula = close(f, (x: Variable, body:Formula) => Forall(x, body))
  def existClose(f: Formula) : Formula = close(f, (x: Variable, body:Formula) => Exists(x, body))

  def test(f: Formula, yes: String, no: String, cf: Formula => Formula) : String = simplify(elimQuantifiers(cf(f))) match {
    case True => yes
    case False => no
    case x => {println(x); println(simplify(x)); throw new RuntimeException("quantifier elimination did not return a truth value")}
  }

  def isValid(f: Formula) : String = test(f, "VALID", "INVALID", (univClose(_)))

  def isSat(f: Formula) : String = test(f, "SAT", "UNSAT", (existClose(_)))

  def equiv(f1: Formula, f2: Formula) : String = isValid(And(Implies(f1, f2), Implies(f2, f1)))

  def main(args: Array[String]) {
	val testSimplify = false
	val testSubst = false
	val testElimQuant = false
	val testFreeVars = false
	val testClose = false
	
    val testInputs = List(
      Forall(Variable("x"), True), 
      Exists(Variable("x"),Variable("x")),
      Exists(Variable("x"),Not(Variable("x"))),
      Variable("x"),
      Not(Variable("x")),
      Forall(Variable("x"),Variable("x")),
      Implies(Variable("x"),Variable("y")),
      Implies(False,Variable("y")),
      Exists(Variable("y"), And(Not(Or(Variable("x"), Variable("y"))), Implies(Variable("y"), Variable("x")))),
      Forall(Variable("x"), Not(Exists(Variable("y"), And(Not(Or(Variable("x"), Variable("y"))), Implies(Variable("y"), Variable("x"))))))
    )

    if (testSimplify) {
	    val testInputSimplify = List(
	    		Or(True, Variable("x")),
	    		And(Variable("y"), False),
	    		Implies(False, Variable("y")),
	    		Or(Variable("z"), Implies(False, Variable("w"))),
	    		And(True, And(True, False)),
	    		Not(Or(And(Or(Not(Or(False, True)), True), False), True)),
	    		And(Variable("x"), True)
	    )
	    testInputSimplify foreach (
	    	(f:Formula) => {
	    		print(f + " --> ")
	    		println(simplify(f))
	    	}
	    )
    }
	
	if (testSubst) {
		println(subst(Variable("x"), Variable("x"), True)) // true
		println(subst(Or(True, Variable("x")), Variable("x"), True)) // true | true
		println(subst(Variable("y"), Variable("x"), False)) // y
		
		// Exception, not a quantifier-free formula
		println(subst(Exists(Variable("y"), Not(Variable("y"))), Variable("y"), False))
	}
	
	if (testElimQuant) {
		val testInputElimQuant = List(
			Exists(Variable("x"), Variable("x")), // (true | false)
			Forall(Variable("x"), Variable("x")), // (true & false)
			Forall(Variable("x"), Implies(Variable("x"), Variable("y"))), // ((true --> y) & (false --> y))
			Exists(Variable("x"), Not(Variable("x"))) // !true | !false
		)
		testInputElimQuant foreach (
	    	(f:Formula) => {
	    		print(f + " --> ")
	    		println(elimQuantifiers(f))
	    	}
	    )
	}
	
	if (testFreeVars) {
		println(freeVars(Exists(Variable("x"), Variable("x")), Set())) // {}
		println(freeVars(Variable("x"), Set())) // {x}
		println(freeVars(Exists(Variable("w"), Or(Variable("z"), Implies(False, Variable("w")))), Set()))
	}
	
	if (testClose) {
		println(univClose(Variable("x"))) // ALL x . x
		println(univClose(Implies(Variable("y"), Variable("x")))) // (ALL x . (ALL y . (y --> x)))
		println(univClose(Exists(Variable("w"), Or(Variable("x"), Variable("w"))))) // (ALL x . (EX w . (x | w)))

		println(existClose(Variable("x"))) // EX x . x
		println(existClose(Implies(Variable("y"), Variable("x")))) // (EX x . (EX y . (y --> x)))
		println(existClose(Exists(Variable("w"), Or(Variable("x"), Variable("w"))))) // (EX x . (EX w . (x | w)))
	}
	
    testInputs foreach ((f:Formula) => println(f + ": " + isSat(f) + " and " + isValid(f)))
  }

  /*
  SHOULD DISPLAY:
  (ALL x . true): SAT and VALID
  (EX x . x): SAT and VALID
  (EX x . !x): SAT and VALID
  x: SAT and INVALID
  !x: SAT and INVALID
  (ALL x . x): UNSAT and INVALID
  (x --> y): SAT and INVALID
  (false --> y): SAT and VALID
  (EX y . (!(x | y) & (y --> x))): SAT and INVALID
  (ALL x . !(EX y . (!(x | y) & (y --> x)))): UNSAT and INVALID
  */

}
