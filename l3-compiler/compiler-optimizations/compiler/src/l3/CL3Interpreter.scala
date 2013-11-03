package l3

import scala.util.parsing.input.Position
import scala.collection.mutable.{ Map => MutableMap }
import SymbolicCL3TreeModule._

/**
 * A tree-based interpreter for the CL₃ language.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CL3Interpreter extends (Tree => Unit) {
  def apply(program: Tree): Unit =
    try {
      eval(program)(Map.empty)
    } catch {
      case _: EvalError =>
        ()
    }

  // Values
  private sealed trait Value {
    override def toString(): String = this match {
      case FunctionV(_, _, _) => "<function>"
      case BlockV(t, c) => "<"+ t +">["+ (c mkString ",") +"]"
      case IntV(i) => i.toString
      case BoolV(b) => if (b) "#t" else "#f"
      case UnitV => "#u"
    }
  }
  private case class FunctionV(args: List[Symbol], body: Tree, env: Env)
               extends Value
  private case class BlockV(tag: Int, contents: Array[Value]) extends Value
  private case class IntV(i: Int) extends Value
  private case class BoolV(b: Boolean) extends Value
  private case object UnitV extends Value

  // Environment
  private type Env = PartialFunction[Symbol, Value]

  // Error handling
  private class EvalError() extends Exception()
  private def error(pos: Position, msg: String): Nothing = {
    Console.println("Error: "+ msg)
    printPos(pos)
    throw new EvalError()
  }
  private def printPos(pos: Position): Unit =
    Console.println("  at "+ pos)

  private final def eval(tree: Tree)(implicit env: Env): Value = tree match {
    case Let(bdgs, body) =>
      eval(body)(Map(bdgs map { case (n, e) => n -> eval(e) } : _*) orElse env)

    case LetRec(funs, body) =>
      val recEnv = MutableMap[Symbol, Value]()
      val env1 = recEnv orElse env
      for (FunDef(name, args, body) <- funs)
        recEnv(name) = FunctionV(args, body, env1)
      eval(body)(env1)

    case If(cond, thenE, elseE) =>
      eval(cond) match {
        case BoolV(false) => eval(elseE)
        case _ => eval(thenE)
      }

    case App(fun, args) =>
      eval(fun) match {
        case FunctionV(cArgs, cBody, cEnv) =>
          if (args.length != cArgs.length)
            error(tree.pos,
                  "expected "+ cArgs.length +" arguments, got "+ args.length)
          try {
            eval(cBody)(Map(cArgs zip (args map eval) : _*) orElse cEnv)
          } catch {
            case e: EvalError =>
              printPos(tree.pos)
              throw e
          }
        case _ => error(tree.pos, "function value expected")
      }

    case Prim(p, args) => (p, args map eval) match {
      case (L3FunctionP, List(FunctionV(_, _, _))) => BoolV(true)
      case (L3FunctionP, _) => BoolV(false)

      case (L3BlockAlloc(t), List(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (L3BlockP, List(BlockV(_, _))) => BoolV(true)
      case (L3BlockP, _) => BoolV(false)
      case (L3BlockTag, List(BlockV(t, _))) => IntV(t)
      case (L3BlockLength, List(BlockV(_, c))) => IntV(c.length)
      case (L3BlockGet, List(BlockV(_, v), IntV(i))) => v(i)
      case (L3BlockSet, List(BlockV(_, v), IntV(i), o)) => v(i) = o; UnitV

      case (L3IntP, List(IntV(_))) => BoolV(true)
      case (L3IntP, _) => BoolV(false)

      case (L3IntAdd, List(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (L3IntSub, List(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (L3IntMul, List(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (L3IntDiv, List(IntV(v1), IntV(v2))) => IntV(v1 / v2)
      case (L3IntMod, List(IntV(v1), IntV(v2))) => IntV(v1 % v2)

      case (L3IntLt, List(IntV(v1), IntV(v2))) => BoolV(v1 < v2)
      case (L3IntLe, List(IntV(v1), IntV(v2))) => BoolV(v1 <= v2)
      case (L3Eq, List(v1, v2)) => BoolV(v1 == v2)
      case (L3Ne, List(v1, v2)) => BoolV(v1 != v2)
      case (L3IntGe, List(IntV(v1), IntV(v2))) => BoolV(v1 >= v2)
      case (L3IntGt, List(IntV(v1), IntV(v2))) => BoolV(v1 > v2)

      case (L3CharRead, List()) => IntV(IO.readChar())
      case (L3CharPrint, List(IntV(c))) => IO.printChar(c); UnitV

      case (L3BoolP, List(BoolV(_))) => BoolV(true)
      case (L3BoolP, _) => BoolV(false)

      case (L3UnitP, List(UnitV)) => BoolV(true)
      case (L3UnitP, _) => BoolV(false)

      case (p, vs) =>
        error(tree.pos,
              "cannot apply primitive "+ p +" to values "+ vs.mkString(", "))
    }

    case Ident(n) => env(n)

    case Lit(IntLit(i)) => IntV(i)
    case Lit(BooleanLit(b)) => BoolV(b)
    case Lit(UnitLit) => UnitV
  }
}
