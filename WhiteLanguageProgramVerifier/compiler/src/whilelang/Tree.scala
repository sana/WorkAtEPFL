package whilelang

/** Terms. The only type in the language is Int. */
sealed abstract class Term
case class Constant(c: Int) extends Term
case class Variable(v: String) extends Term
case class Plus(t1: Term, t2: Term) extends Term
case class Minus(t1: Term, t2: Term) extends Term
case class Times(c: Int, t: Term) extends Term
case class Divide(t: Term, c: Int) extends Term
 
/** Boolean expressions, used as conditions and in assertions/assumptions. */
sealed abstract class BoolExpr
case class Equal(t1: Term, t2: Term) extends BoolExpr
case class LessThan(t1: Term, t2: Term) extends BoolExpr
case class GreaterThan(t1: Term, t2: Term) extends BoolExpr
case class Negation(F: BoolExpr) extends BoolExpr
case class And(f1: BoolExpr, f2: BoolExpr) extends BoolExpr
case class Or(f1: BoolExpr, f2: BoolExpr) extends BoolExpr
 
/** Program commands */
sealed abstract class Command
case class Assignment(v: String, t: Term) extends Command
case class IfThenElse(cond: BoolExpr, ifcom: Command, elsecom: Command) extends Command
case class Block(coms: List[Command]) extends Command
case class WhileLoop(cond: BoolExpr, com: Command) extends Command
case class Skip() extends Command
case class Assume(expr: BoolExpr) extends Command
case class Assert(expr: BoolExpr) extends Command
case class Choice(choice1: Command, choice2: Command) extends Command
case class Havoc(v: Variable) extends Command
case class Loop(com: Command) extends Command

/** Pretty-prints programs. */
object TreePrinter {
  var level: Int = 0
  def incLevel = { level = level + 1 }
  def decLevel = { level = level - 1 }
  def printInc = { print((0 to level).map(l => "").mkString("  ")) }
  
  def apply(stat: Command): Unit = stat match {
    case Block(sts) => sts.foreach(prt(_))
    case other => prt(other)
  }

  private def prt(stat: Command): Unit = {
    def asBlock(st: Command): Unit = st match {
      case Block(sts) => {
        println(" {")
        incLevel
        sts.foreach(prt(_))
        decLevel
        printInc
        print("}")
      }

      case _ => {
        println()
        incLevel
        prt(st)
        decLevel
      }
    }

    stat match {
      case Assignment(varID, expr) => {
        printInc
        print(varID + " = ")
        prt(expr)
        println(";")
      }
      case IfThenElse(expr, then, elze) => {
        printInc
        print("if ("); prt(expr); print(")")
        asBlock(then)
       
        elze match {
          case Skip() => println
          case _ => {
            print(" else")
            asBlock(elze)
            println
          }
        }
      }
      case WhileLoop(expr, body) => {
        printInc
        print("while ("); prt(expr); print(")")
        asBlock(body)
        println
      }
      case Block(body) => {
        printInc
        println("{")
        incLevel
        body.foreach(prt(_))
        decLevel
        printInc
        println("}")
      }
      case Assume(be) => {
        printInc
        print("assume("); prt(be); println(");")
      }
      case Assert(be) => {
        printInc
        print("assert("); prt(be); println(");")
      }
      case Choice(c1, c2) => {
        printInc
        println("(")
        incLevel
        prt(c1)
        decLevel
        println("[]")
        incLevel
        prt(c2)
        decLevel
        printInc
        println(")")
      }
      case Loop(c) => {
        printInc
        print("loop")
        asBlock(c)
        println
      }
      case Havoc(v) => {
        printInc
        print("havoc("); prt(v); println(");")
      }
      case Skip() => ;
    }
  }
  
  def prt(term: Term): Unit = term match {
    case Variable(varID) => print(varID)
    case Constant(value) => print(value)
    case Plus(lhs, rhs) => { print("("); prt(lhs); print(" + "); prt(rhs); print(")") }
    case Minus(lhs, rhs) => { print("("); prt(lhs); print(" - "); prt(rhs); print(")") }
    case Times(lhs, rhs) => { print("(" + lhs + " * "); prt(rhs); print(")") }
    case Divide(lhs, rhs) => { print("("); prt(lhs); print(" / " + rhs + ")") }
  }

  def prt(expr: BoolExpr): Unit = expr match {
    case Equal(lhs, rhs) => { print("("); prt(lhs); print(" == "); prt(rhs); print(")") }
    case GreaterThan(lhs, rhs) => { print("("); prt(lhs); print(" > "); prt(rhs); print(")") }
    case LessThan(lhs, rhs) => { print("("); prt(lhs); print(" < "); prt(rhs); print(")") }
    case And(lhs, rhs) => { print("("); prt(lhs); print(" && "); prt(rhs); print(")") }
    case Or(lhs, rhs) => { print("("); prt(lhs); print(" || "); prt(rhs); print(")") }
    case Negation(expr) => { print("!"); prt(expr) }
  }
}

sealed abstract class Tree // makes Ant happier 
