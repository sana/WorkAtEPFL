package whilelang
 
object Formulas {
  /** All terms. */
  sealed abstract class Term {
    override def toString: String = this match {
      case Const(value) => {
        if(value < 0) {
          "(~ " + (-value).toString + ")"
        } else {
          value.toString
        }
      }
      case Var(id) => id
      case Ite(c,t,e) => "(ite " + c + " " + t + " " + e + ")"
      case Plus(t1,t2) => "(+ " + t1 + " " + t2 + ")"
      case Minus(t1,t2) => "(- " + t1 + " " + t2 + ")"
      case Times(cst,t) => "(* " + Const(cst) + " " + t + ")"
      case Div(t,cst) => "(/ " + t + " " + Const(cst) + ")"
      case Mod(t,cst) => "(- " + t + " (* " + Const(cst) + "(/ " + t + " " + Const(cst) + ")))"
    }
  }
 
  case class Var(id: String) extends Term
  case class Const(value: Int) extends Term
  case class Ite(cond: Formula, then: Term, elze: Term) extends Term // Can express a conditional value for a term. Similar to the notation (cond ? a : b) in, eg. C.
  case class Plus(lhs: Term, rhs: Term) extends Term
  case class Minus(lhs: Term, rhs: Term) extends Term
  case class Times(cst: Int, rhs: Term) extends Term
  case class Div(lhs: Term, cst: Int) extends Term
  case class Mod(lhs: Term, cst: Int) extends Term
 
  /** All formulas */
  sealed abstract class Formula {
    override def toString: String = this match {
      case Equals(l,r) => "(= " + l.toString + " " + r.toString + ")"
      case Not(f) => "(not " + f.toString + ")"
      case Or(fs) => "(or " + fs.map(_.toString).mkString(" ") + ")"
      case And(fs) => "(and " + fs.map(_.toString).mkString(" ") + ")"
      case Implies(lhs,rhs) => "(implies " + lhs + " " + rhs + ")"
      case True() => "true"
      case False() => "false"
      case BoolVar(id) => id
      case LessThan(l,r) => "(< " + l.toString + " " + r.toString + ")"
      case GreaterThan(l,r) => "(> " + l.toString + " " + r.toString + ")"
    }
  }
 
  case class Equals(lhs: Term, rhs: Term) extends Formula
  case class Not(form: Formula) extends Formula
  case class Or(forms: List[Formula]) extends Formula
  case class And(forms: List[Formula]) extends Formula
  case class Implies(lhs: Formula, rhs: Formula) extends Formula
  case class True() extends Formula
  case class False() extends Formula
  case class BoolVar(id: String) extends Formula // Boolean variables. You may not need them.
  case class LessThan(lhs: Term, rhs: Term) extends Formula
  case class GreaterThan(lhs: Term, rhs: Term) extends Formula

  /** Free variables */
  def varsInForm(form: Formula): List[String] = {
    def vif(f: Formula, acc: List[String]): List[String] = f match {
      case And(fs) => fs.flatMap(vif(_,Nil)) ::: acc
      case Or(fs)  => fs.flatMap(vif(_,Nil)) ::: acc
      case Not(f)  => vif(f,acc)
      case Implies(f1,f2) => vif(f1, vif(f2, acc))
      case Equals(l,r) => varsInTerm(l) ::: varsInTerm(r)
      case True() => acc
      case False() => acc
      case BoolVar(_) => acc
      case LessThan(lhs, rhs) => varsInTerm(lhs) ::: varsInTerm(rhs)
      case GreaterThan(lhs, rhs) => varsInTerm(lhs) ::: varsInTerm(rhs)
    }
    vif(form,Nil).distinct.reverse
  }
  def varsInTerm(term: Term): List[String] = {
    def vit(t: Term, acc: List[String]): List[String] = t match {
      case Var(id) => id :: acc
      case Plus(lhs,rhs) => vit(lhs, vit(rhs, acc))
      case Minus(lhs,rhs) => vit(lhs, vit(rhs, acc))
      case Times(_,rhs) => vit(rhs, acc)
      case Div(lhs,_) => vit(lhs, acc)
      case Mod(lhs,_) => vit(lhs, acc)
      case Ite(c,t,e) => varsInForm(c) ::: vit(t, vit(e, acc))
      case Const(_) => acc
    }
    vit(term,Nil).distinct.reverse
  }
  def boolVarsInForm(form: Formula): List[String] = {
    def vif(f: Formula, acc: List[String]): List[String] = f match {
      case And(fs) => fs.flatMap(vif(_,Nil)) ::: acc
      case Or(fs)  => fs.flatMap(vif(_,Nil)) ::: acc
      case Not(f)  => vif(f,acc)
      case Implies(lhs,rhs) => vif(lhs, vif(rhs,acc))
      case BoolVar(v) => v :: acc
      case True() | False() | Equals(_,_) => acc
      case LessThan(lhs, rhs) => acc
      case GreaterThan(lhs, rhs) => acc
    }
    vif(form,Nil).distinct.reverse
  }
 
  /** Attempts to use Z3 to determine whether the formula is satisfiable.
   * If the result is None, Z3 couldn't determine the result. Else the
   * result is returned in Some(...). */
  def isSat(f: Formula): Option[Boolean] = {
    val process = java.lang.Runtime.getRuntime.exec("./z3.sh -smt")
    val out = new java.io.PrintStream(process.getOutputStream)
    out.println(benchmarkString(f))
    out.flush
    out.close
    val in = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
    val ans = in.readLine
    ans match {
      case "sat" => Some(true)
      case "unsat" => Some(false)
      case _ => {
        println("Z3 complained with: " + ans)
        None
      }
    }
  }
 
  def benchmarkString(formula: Formula): String = {
    var str = "(benchmark sav11\n  :logic AUFNIRA\n  :status unknown\n"
 
    val vars = varsInForm(formula)

    if(!vars.isEmpty) {
      str = str + ":extrafuns ( "
      vars.foreach(id => {
        str = str + "(" + id + " Int) "
      })
      str = str + ")\n"
    }
 
    val bools = boolVarsInForm(formula)
    if(!bools.isEmpty) {
      str = str + ":extrapreds ( "
      bools.foreach(id => {
        str = str + "(" + id + ") "
      })
      str = str + ")\n"
    }

    str = str + "  :formula \n"
    str = str + formula.toString + "\n"
    str = str + "  )\n"

    str
  }
}
