package whilelang

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

object  VCGen {
  def compoVCG(prog: Command): Formulas.Formula = {
    /* Generate a formula using compositional VCG */ 
    val vars = compoVCGimpl(prog, varsInProgram(prog), List())
	val left_formula = Formulas.And(List(Formulas.Equals(Formulas.Var("error"), Formulas.Const(0)), vars))
	val right_formula = Formulas.Equals(Formulas.Var("error_p"), Formulas.Const(0))
    Formulas.Not(Formulas.Implies(left_formula, right_formula))
  }

  def varsInProgram(prog: Command): List[String] = {
    def varsInTerm(t: Term, acc: List[String]): List[String] = t match {
      case Constant(c) => acc
      case Variable(v) => List(v) ::: acc
      case Plus(t1, t2) => varsInTerm(t1, acc) ::: varsInTerm(t2, acc)
      case Minus(t1, t2) => varsInTerm(t1, acc) ::: varsInTerm(t2, acc)
      case Times(c, t) => varsInTerm(t, acc)
      case Divide(t, c) => varsInTerm(t, acc)
    }

    def varsInBoolExpr(t: BoolExpr, acc: List[String]): List[String] = t match {
      case Equal(t1, t2) => varsInTerm(t1, acc) ::: varsInTerm(t2, acc)
      case LessThan(t1, t2) => varsInTerm(t1, acc) ::: varsInTerm(t2, acc)
      case GreaterThan(t1, t2) => varsInTerm(t1, acc) ::: varsInTerm(t2, acc)
      case Negation(f) => varsInBoolExpr(f, acc)
      case And(f1, f2) => varsInBoolExpr(f1, acc) ::: varsInBoolExpr(f2, acc)
      case Or(f1, f2) => varsInBoolExpr(f1, acc) ::: varsInBoolExpr(f2, acc)
    }

    def varsInProgramAux(prog: Command, acc: List[String]): List[String] = prog match {
      case Assignment(v, t) => List(v) ::: varsInTerm(t, Nil) ::: acc
      case IfThenElse(cond, ifcom, elsecom) => varsInBoolExpr(cond, Nil) ::: varsInProgramAux(ifcom, Nil) ::: varsInProgramAux(elsecom, Nil) ::: acc
      case Block(coms) => {
        var list = new ListBuffer[String]()
        for (com <- coms)
          list = list ++ varsInProgramAux(com, Nil)
        list.toList ::: acc
      }
      case WhileLoop(cond, com) => varsInBoolExpr(cond, Nil) ::: varsInProgramAux(com, Nil) ::: acc
      case Skip() => acc
      case Assume(expr) => varsInBoolExpr(expr, Nil) ::: acc
      case Assert(expr) => varsInBoolExpr(expr, Nil) ::: acc
      case Choice(choice1, choice2) => varsInProgramAux(choice1, Nil) ::: varsInProgramAux(choice2, Nil) ::: acc
      case Havoc(v) => List(v.v) ::: acc
      case Loop(command) => varsInProgramAux(command, acc)
    }
    "error" :: varsInProgramAux(prog, Nil).distinct
  }

  def adaptTerm(t: Term): Formulas.Term = {
    t match {
      case Constant(c: Int) => Formulas.Const(c)
      case Variable(v1: String) => Formulas.Var(v1)
      case Plus(t1: Term, t2: Term) => Formulas.Plus(adaptTerm(t1), adaptTerm(t2))
      case Minus(t1: Term, t2: Term) => Formulas.Minus(adaptTerm(t1), adaptTerm(t2))
      case Times(c: Int, t1: Term) => Formulas.Times(c, adaptTerm(t1))
      case Divide(t1: Term, c: Int) => Formulas.Div(adaptTerm(t1), c)
    }
  }

  def adaptBoolExpr(expr: BoolExpr): Formulas.Formula = {
    expr match {
      case Equal(t1: Term, t2: Term) => Formulas.Equals(adaptTerm(t1), adaptTerm(t2))
      case LessThan(t1: Term, t2: Term) => Formulas.LessThan(adaptTerm(t1), adaptTerm(t2))
      case GreaterThan(t1: Term, t2: Term) => Formulas.GreaterThan(adaptTerm(t1), adaptTerm(t2))
      case Negation(f: BoolExpr) => Formulas.Not(adaptBoolExpr(f))
      case And(f1: BoolExpr, f2: BoolExpr) => Formulas.And(List(adaptBoolExpr(f1), adaptBoolExpr(f2)))
      case Or(f1: BoolExpr, f2: BoolExpr) => Formulas.Or(List(adaptBoolExpr(f1), adaptBoolExpr(f2)))
    }
  }

  private def identityVarsTransformation(vars: List[String]): List[Formulas.Equals] = {
    vars match {
      case List() => List()
      case x :: xs => Formulas.Equals(Formulas.Var(x + "_p"), Formulas.Var(x)) :: identityVarsTransformation(xs)
    }
  }

  private def removeConstraint(vars: List[String], v: Variable): List[Formulas.Equals] = {
    vars match {
      case List() => List()
      case x :: xs => {
        if (x == v.v)
          removeConstraint(xs, v)
        else
          Formulas.Equals(Formulas.Var(x + "_p"), Formulas.Var(x)) :: removeConstraint(xs, v)
      }
    }
  }

  private def replaceTermWithFresh(t: Formulas.Term, freshVars: Map[String, String], suffix: String): Formulas.Term = {
    t match {
      case Formulas.Var(id) => {
        if (id.endsWith(suffix) && freshVars.contains(id.dropRight(suffix.length())))
           Formulas.Var(freshVars(id.dropRight(suffix.length())))
        else
          t
      }
      case Formulas.Const(value) => t
      case Formulas.Ite(cond, then, elze) => Formulas.Ite(replaceFormulaWithFresh(cond, freshVars, suffix),
        replaceTermWithFresh(then, freshVars, suffix), replaceTermWithFresh(elze, freshVars, suffix))
      case Formulas.Plus(lhs, rhs) => Formulas.Plus(replaceTermWithFresh(lhs, freshVars, suffix), replaceTermWithFresh(rhs, freshVars, suffix))
      case Formulas.Minus(lhs, rhs) => Formulas.Minus(replaceTermWithFresh(lhs, freshVars, suffix), replaceTermWithFresh(rhs, freshVars, suffix))
      case Formulas.Times(cst, rhs) => Formulas.Times(cst, replaceTermWithFresh(rhs, freshVars, suffix))
      case Formulas.Div(lhs, cst) => Formulas.Div(replaceTermWithFresh(lhs, freshVars, suffix), cst)
      case Formulas.Mod(lhs, cst) => Formulas.Mod(replaceTermWithFresh(lhs, freshVars, suffix), cst)
    }
  }

  private def replaceFormulaWithFresh(f: Formulas.Formula, freshVars: Map[String, String], suffix: String): Formulas.Formula = {
    f match {
      case Formulas.Equals(lhs, rhs) => Formulas.Equals(replaceTermWithFresh(lhs, freshVars, suffix), replaceTermWithFresh(rhs, freshVars, suffix))
      case Formulas.Not(form) => Formulas.Not(replaceFormulaWithFresh(form, freshVars, suffix))
      case Formulas.Or(forms) => {
    	  if (forms.length == 0)
    	 	Formulas.True()
    	  else
    	    Formulas.Or(forms.map(x => replaceFormulaWithFresh(x, freshVars, suffix)))
      }
      case Formulas.And(forms) => {
    	  if (forms.length == 0)
    	 	Formulas.True()
    	  else
    		Formulas.And(forms.map(x => replaceFormulaWithFresh(x, freshVars, suffix)))
      }
      case Formulas.Implies(lhs, rhs) => Formulas.Implies(replaceFormulaWithFresh(lhs, freshVars, suffix), replaceFormulaWithFresh(rhs, freshVars, suffix))
      case Formulas.True() => Formulas.True()
      case Formulas.False() => Formulas.False()
      case Formulas.BoolVar(id) => {
        if (id.endsWith(suffix))
          Formulas.BoolVar(freshVars(id.dropRight(suffix.length())))
        else
          f
      }
      case Formulas.LessThan(lhs, rhs) => Formulas.LessThan(replaceTermWithFresh(lhs, freshVars, suffix), replaceTermWithFresh(rhs, freshVars, suffix))
      case Formulas.GreaterThan(lhs, rhs) => Formulas.GreaterThan(replaceTermWithFresh(lhs, freshVars, suffix), replaceTermWithFresh(rhs, freshVars, suffix))
    }
  }

  def combineFormulas(old_f: Formulas.Formula, new_f: Formulas.Formula, freshVars: Map[String, String]): Formulas.Formula = {
    if (old_f == null)
      new_f
    else
    {
    	//println(replaceFormulaWithFresh(old_f, freshVars, "_p"))
    	//println(replaceFormulaWithFresh(new_f, freshVars, ""))
        Formulas.And(List(replaceFormulaWithFresh(old_f, freshVars, "_p"), replaceFormulaWithFresh(new_f, freshVars, "")))
    }
  }

  def compoVCGimpl(prog: Command, vars: List[String], pc: List[Formulas.Formula]): Formulas.Formula = {

   prog match {
      case Assignment(v: String, t: Term) => {
        // x = f(ex, ey) && y = ey && P
        val f = adaptTerm(t)

        val list = new ListBuffer[Formulas.Equals]()
        for (x <- vars)
        {
          if (x == v)
            list += Formulas.Equals(Formulas.Var(x + "_p"), f);
          else
            list += Formulas.Equals(Formulas.Var(x + "_p"), Formulas.Var(x))
        }
        Formulas.And(list.toList ++ pc)
      }

      case Block(coms: List[Command]) => {
    	/*
    	 * We combine the formulas for-each two consecutive formulas
    	 * in the block; the combination function replaces v' in the first
    	 * formula with v_fresh and v in the second with v_fresh, in order
    	 * to connect these two formulas
    	 */
        var acc: Formulas.Formula = Formulas.And(identityVarsTransformation(vars))
        for (cmd <- coms)
        {
          var aux = compoVCGimpl(cmd, vars, pc)
          acc = combineFormulas(acc, aux, freshVariableNames(vars))
        }
        Formulas.And(acc :: pc)
      }

      // Identify variables transformation and path condition
      case Skip() => Formulas.And(identityVarsTransformation(vars) ++ pc)

      case Assume(expr) => {
    	// x = ex && y = ey && PC && F[x = ex, y = ey]
        Formulas.And(identityVarsTransformation(vars) ++ List(adaptBoolExpr(expr)) ++ pc)
      }

      case Assert(expr) => {
    	  /*
    	   * (e = 1 && e' = 1) ||
    	   * (e = 0 && F(x) && e' = 0) ||
    	   * (e = 0 && not(F(x)) && e' = 1)
    	   */
    	  val error_branch = Formulas.And(List(
    	    Formulas.Equals(Formulas.Var("error"), Formulas.Const(1)),
    	 	Formulas.Equals(Formulas.Var("error_p"), Formulas.Const(1))))
    	 	
    	  val valid_branch = Formulas.And(List(
    	    Formulas.Equals(Formulas.Var("error"), Formulas.Const(0)),
    	    adaptBoolExpr(expr),
    	 	Formulas.Equals(Formulas.Var("error_p"), Formulas.Const(0))))
    	 	
    	  val invalid_branch = Formulas.And(List(
    	    Formulas.Equals(Formulas.Var("error"), Formulas.Const(0)),
    	    Formulas.Not(adaptBoolExpr(expr)),
    	 	Formulas.Equals(Formulas.Var("error_p"), Formulas.Const(1))))

    	  Formulas.And(identityVarsTransformation(vars.filterNot(x => x == "error")) ++ List(Formulas.Or(List(valid_branch, invalid_branch, error_branch))) ++ pc)
      }

      case Choice(choice1: Command, choice2: Command) => {
    	// (xk = ex1 && yk = ey1 && PC1) || (xk = ex2 && yk = ey2 && PC2)
        Formulas.Or(List(compoVCGimpl(choice1, vars, pc), compoVCGimpl(choice2, vars, pc)))
      }

      case Havoc(v: Variable) => {
        Formulas.And(removeConstraint(vars, v) ++ pc)
      }
      
      case _ => throw new Exception("Inconsistent state, please report!")
    }
  }
  
  private val counters = scala.collection.mutable.HashMap[String,Int]()

  private def freshName(prefix: String): String = {
    val count = counters.getOrElse(prefix, 1)
    counters.put(prefix, count + 1)
    prefix + "_" + count
  }

  private def freshVariableNames(vars: List[String]): Map[String, String] = {
    vars match {
      case List() => Map[String, String]()
      case x :: xs => freshVariableNames(xs) + (x -> freshName(x))
    }
  }
}
