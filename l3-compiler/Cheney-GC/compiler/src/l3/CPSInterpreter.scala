package l3

import scala.collection.mutable.{ Map => MutableMap }

/**
 * A tree-based interpreter for the CPS languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

abstract class CPSInterpreter[T <: CPSTreeModule](protected val treeModule: T) extends (T#Tree => Unit) {
  import treeModule._

  def apply(tree: T#Tree): Unit =
    eval(tree, emptyEnv)

  protected type Env = PartialFunction[Name, Value]
  protected val emptyEnv: Env = Map.empty

  protected sealed trait Value
  protected case class FunctionV(retK: Name, args: Seq[Name], body: Tree, env: Env)
                 extends Value
  protected case class ContV(args: Seq[Name], body: Tree, env: Env)
                 extends Value

  protected def evalLit(l: Literal): Value
  protected def evalPrim(p: ValuePrimitive, args: Seq[Value]): Value
  protected def evalCond(p: TestPrimitive, args: Seq[Value]): Boolean

  protected final def eval(tree: T#Tree, env: Env): Unit = tree match {
    case LetL(name, value, body) =>
      eval(body, Map(name -> evalLit(value)) orElse env)

    case LetP(name, prim, args, body) =>
      eval(body, Map(name -> evalPrim(prim, args map env)) orElse env)

    case LetK(name, args, contBody, body) =>
      eval(body, Map(name -> ContV(args, contBody, env)) orElse env)

    case LetF(funs, body) =>
      val recEnv = MutableMap[Name, Value]()
      val env1 = recEnv orElse env
      for (FunDef(name, retK, args, fbody) <- funs)
        recEnv(name) = FunctionV(retK, args, fbody, env1)
      eval(body, env1)

    case AppK(cont, args) =>
      env(cont) match {
        case ContV(cArgs, cBody, cEnv) =>
          assume(cArgs.length == args.length)
          eval(cBody, Map(cArgs zip (args map env) : _*) orElse cEnv)
      }

    case AppF(fun, retK, args) =>
      env(fun) match {
        case FunctionV(cRetK, cArgs, cBody, cEnv) =>
          assume(cArgs.length == args.length)
          eval(cBody, Map(((cRetK +: cArgs) zip ((retK +: args) map env)) : _*)
               orElse cEnv)
      }

    case If(cond, args, thenK, elseK) =>
      env(if (evalCond(cond, args map env)) thenK else elseK) match {
        case ContV(Seq(), cBody, cEnv) =>
          eval(cBody, cEnv)
      }

    case Halt =>
      env
  }
}

/**
 * Interpreter for "high-level" CPS programs (before the
 * data-representation phase).
 */
object SymbolicCPSInterpreter extends CPSInterpreter(SymbolicCPSTreeModule) {
  import treeModule._

  case class BlockV(tag: Int, contents: Array[Value]) extends Value
  case class IntV(value: Int) extends Value
  case class BoolV(value: Boolean) extends Value
  case object UnitV extends Value

  protected def evalLit(l: Literal): Value = l match {
    case IntLit(i) => IntV(i)
    case BooleanLit(b) => BoolV(b)
    case UnitLit => UnitV
  }

  protected def evalPrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (L3BlockAlloc(t), Seq(IntV(i))) => BlockV(t, Array.fill(i)(UnitV))
      case (L3BlockTag, Seq(BlockV(t, _))) => IntV(t)
      case (L3BlockLength, Seq(BlockV(_, c))) => IntV(c.length)
      case (L3BlockGet, Seq(BlockV(_, v), IntV(i))) => v(i)
      case (L3BlockSet, Seq(BlockV(_, v), IntV(i), o)) => v(i) = o; UnitV

      case (L3IntAdd, Seq(IntV(v1), IntV(v2))) => IntV(v1 + v2)
      case (L3IntSub, Seq(IntV(v1), IntV(v2))) => IntV(v1 - v2)
      case (L3IntMul, Seq(IntV(v1), IntV(v2))) => IntV(v1 * v2)
      case (L3IntDiv, Seq(IntV(v1), IntV(v2))) => IntV(v1 / v2)
      case (L3IntMod, Seq(IntV(v1), IntV(v2))) => IntV(v1 % v2)

      case (L3CharRead, Seq()) => IntV(IO.readChar())
      case (L3CharPrint, Seq(IntV(c))) => IO.printChar(c); UnitV
    }

  protected def evalCond(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (L3FunctionP, Seq(FunctionV(_, _, _, _))) => true
      case (L3FunctionP, _) => false

      case (L3BlockP, Seq(BlockV(_, _))) => true
      case (L3BlockP, _) => false

      case (L3IntP, Seq(IntV(_))) => true
      case (L3IntP, _) => false
      case (L3IntLt, Seq(IntV(v1), IntV(v2))) => v1 < v2
      case (L3IntLe, Seq(IntV(v1), IntV(v2))) => v1 <= v2
      case (L3IntGe, Seq(IntV(v1), IntV(v2))) => v1 >= v2
      case (L3IntGt, Seq(IntV(v1), IntV(v2))) => v1 > v2

      case (L3BoolP, Seq(BoolV(_))) => true
      case (L3BoolP, _) => false

      case (L3UnitP, Seq(UnitV)) => true
      case (L3UnitP, _) => false

      case (L3Eq, Seq(v1, v2)) => v1 == v2
      case (L3Ne, Seq(v1, v2)) => v1 != v2
    }
}

/**
 * Interpreter for "low-level" CPS programs (after the
 * data-representation phase).
 */
object SymbolicCPSInterpreterLow extends CPSInterpreter(SymbolicCPSTreeModuleLow) {
  import treeModule._

  case class BlockV(addr: Int, tag: Int, contents: Array[Value]) extends Value
  case class IntV(value: Int) extends Value
  case object UndefV extends Value

  private var nextBlockAddr = 0
  private def allocBlock(tag: Int, contents: Array[Value]): BlockV = {
    val block = BlockV(nextBlockAddr, tag, contents)
    nextBlockAddr += 4
    block
  }

  private implicit def valueToInt(v: Value): Int = v match {
    case BlockV(addr, _, _) => addr
    case IntV(value) => value
    case FunctionV(_, _, _, _) => 0
  }

  protected def evalLit(l: Literal): Value = IntV(l)

  protected def evalPrim(p: ValuePrimitive, args: Seq[Value]): Value =
    (p, args) match {
      case (CPSAdd, Seq(v1, v2)) => IntV(v1 + v2)
      case (CPSSub, Seq(v1, v2)) => IntV(v1 - v2)
      case (CPSMul, Seq(v1, v2)) => IntV(v1 * v2)
      case (CPSDiv, Seq(v1, v2)) => IntV(v1 / v2)
      case (CPSMod, Seq(v1, v2)) => IntV(v1 % v2)

      case (CPSArithShiftL, Seq(v1, v2)) => IntV(v1 << v2)
      case (CPSArithShiftR, Seq(v1, v2)) => IntV(v1 >> v2)
      case (CPSAnd, Seq(v1, v2)) => IntV(v1 & v2)
      case (CPSOr, Seq(v1, v2)) => IntV(v1 | v2)

      case (CPSCharRead, Seq()) => IntV(IO.readChar().toInt)
      case (CPSCharPrint, Seq(c)) => IO.printChar(c.toChar); UndefV

      case (CPSBlockAlloc(t), Seq(s)) => allocBlock(t, Array.fill(s)(UndefV))
      case (CPSBlockTag, Seq(BlockV(_, t, _))) => IntV(t)
      case (CPSBlockSize, Seq(BlockV(_, _, c))) => IntV(c.length)
      case (CPSBlockGet, Seq(BlockV(_, _, c), i)) => c(i)
      case (CPSBlockSet, Seq(BlockV(_, _, c), i, v)) => c(i) = v; UndefV

      case (CPSCopy, Seq(o)) => o
    }

  protected def evalCond(p: TestPrimitive, args: Seq[Value]): Boolean =
    (p, args) match {
      case (CPSLt, List(v1, v2)) => v1 < v2
      case (CPSLe, List(v1, v2)) => v1 <= v2
      case (CPSEq, List(v1, v2)) => v1 == v2
      case (CPSNe, List(v1, v2)) => v1 != v2
      case (CPSGe, List(v1, v2)) => v1 >= v2
      case (CPSGt, List(v1, v2)) => v1 > v2
    }
}
