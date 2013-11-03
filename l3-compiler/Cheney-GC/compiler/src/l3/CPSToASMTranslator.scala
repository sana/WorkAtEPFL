package l3

import collection.immutable.Set
import collection.mutable.{ Map => MutableMap }

import BitTwiddling.signExtend18
import RegisterCPSTreeModule._
import LabeledASMInstructionModule._

/**
 * An ASM code generator for CPS/L₃.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */
object CPSToASMTranslator extends (Tree => LabeledProgram) {
  def apply(tree: Tree): LabeledProgram = {
    import ASMRegisterFile.{ Lb, Ob }
    val I2 = ASMRegisterFile.in(2)
    val I3 = ASMRegisterFile.in(3)
    val O3 = ASMRegisterFile.out(3)

    val conts = MutableMap[Symbol, Tree]()

    def nl(i: Instruction): LabeledInstruction =
      LabeledInstruction(Set.empty, i)
    def labeled(label: Symbol, code: LabeledProgram): LabeledProgram =
      code match {
        case Seq(LabeledInstruction(labels, i1), rest @ _*) =>
          LabeledInstruction(labels + label, i1) +: rest
      }

    def contOrJump(l: Symbol): LabeledProgram =
      (((conts remove l) map { t => labeled(l, linearize(t)) })
       getOrElse Seq(nl(JI(l))))

    def funPrelude(body: Tree): LabeledProgram = {
      def indexFrom(b: ASMBaseRegister)(r: ASMRegister): Option[Int] =
        if (r.base == b) Some(r.index) else None

      def maybeAlloc(b: ASMBaseRegister, rs: Set[ASMRegister], minSize: Int) = {
        val basedRS = rs flatMap indexFrom(b)
        if (basedRS.nonEmpty)
          Seq(nl(RALO(b, (basedRS.max + 1) max minSize)))
        else
          Seq()
      }

      val regs = usedRegs(body)
      maybeAlloc(Lb, regs, 0) ++ maybeAlloc(Ob, regs, 16)
    }

    def condJump(p: CPSTestPrimitive, a: ASMRegister, b: ASMRegister, k: Symbol) =
      p match {
        case CPSLt => nl(JLT(a, b, k))
        case CPSLe => nl(JLE(a, b, k))
        case CPSEq => nl(JEQ(a, b, k))
        case CPSNe => nl(JNE(a, b, k))
        case CPSGe => nl(JGE(a, b, k))
        case CPSGt => nl(JGT(a, b, k))
      }

    def usedRegs(tree: Tree): Set[ASMRegister] = {
      def regIn(n: Name): Set[ASMRegister] = n match {
        case Reg(r) => Set(r)
        case Label(_) => Set.empty
      }

      def regsIn(ns: Seq[Name]): Set[ASMRegister] =
        ((Set.empty : Set[ASMRegister]) /: (ns map regIn))(_ | _)

      tree match {
        case LetL(Reg(a), _, body) =>
          Set(a) | usedRegs(body)
        case LetP(Reg(a), _, args, body) =>
          Set(a) | regsIn(args) | usedRegs(body)
        case LetK(Label(_), args, contBody, body) =>
          regsIn(args) | usedRegs(contBody) | usedRegs(body)
        case LetF(_, body) =>
          usedRegs(body)
        case AppK(k, args) =>
          regIn(k) | regsIn(args)
        case AppF(f, retK, args) =>
          regIn(f) | regIn(retK) | regsIn(args)
        case If(_, args, tk, ek) =>
          regsIn(args) | regIn(tk) | regIn(ek)
        case Halt =>
          Set.empty
      }
    }

    def invertCond(p: CPSTestPrimitive): CPSTestPrimitive = p match {
      case CPSLt => CPSGe
      case CPSLe => CPSGt
      case CPSEq => CPSNe
      case CPSNe => CPSEq
      case CPSGe => CPSLt
      case CPSGt => CPSLe
    }

    def linearize(tree: Tree): LabeledProgram = {
      tree match {
        case LetL(Reg(a), v, body) if (signExtend18(v & 0x3FFFF) == v) =>
          nl(LDLO(a, IntC(v))) +: linearize(body)
        case LetL(Reg(a), v, body) =>
          val lsb16 = v & 0xFFFF
          val msb16 = v >>> 16
          nl(LDLO(a, IntC(lsb16))) +: nl(LDHI(a, msb16)) +: linearize(body)

        case LetP(Reg(a), CPSAdd, List(Reg(b), Reg(c)), body) =>
          nl(ADD(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSSub, List(Reg(b), Reg(c)), body) =>
          nl(SUB(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSMul, List(Reg(b), Reg(c)), body) =>
          nl(MUL(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSDiv, List(Reg(b), Reg(c)), body) =>
          nl(DIV(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSMod, List(Reg(b), Reg(c)), body) =>
          nl(MOD(a, b, c)) +: linearize(body)

        case LetP(Reg(a), CPSArithShiftL, List(Reg(b), Reg(c)), body) =>
          nl(ASL(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSArithShiftR, List(Reg(b), Reg(c)), body) =>
          nl(ASR(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSAnd, List(Reg(b), Reg(c)), body) =>
          nl(AND(a, b, c)) +: linearize(body)
        case LetP(Reg(a), CPSOr, List(Reg(b), Reg(c)), body) =>
          nl(OR(a, b, c)) +: linearize(body)

        case LetP(Reg(a), CPSCharRead, List(), body) =>
          nl(CREA(a)) +: linearize(body)
        case LetP(_, CPSCharPrint, List(Reg(a)), body) =>
          nl(CPRI(a)) +: linearize(body)

        case LetP(Reg(a), CPSBlockAlloc(t), List(Reg(b)), body) =>
          nl(BALO(a, b, t)) +: linearize(body)
        case LetP(Reg(a), CPSBlockTag, List(Reg(b)), body) =>
          nl(BTAG(a, b)) +: linearize(body)
        case LetP(Reg(a), CPSBlockSize, List(Reg(b)), body) =>
          nl(BSIZ(a, b)) +: linearize(body)
        case LetP(Reg(a), CPSBlockGet, List(Reg(b), Reg(c)), body) =>
          nl(BGET(a, b, c)) +: linearize(body)
        case LetP(_, CPSBlockSet, List(Reg(a), Reg(b), Reg(c)), body) =>
          nl(BSET(c, a, b)) +: linearize(body)

        case LetP(Reg(a), CPSCopy, List(Reg(b)), body) if a == b =>
          linearize(body)
        case LetP(Reg(a), CPSCopy, List(Reg(b)), body) =>
          nl(MOVE(a, b)) +: linearize(body)
        case LetP(Reg(a), CPSCopy, List(Label(l)), body) =>
          nl(LDLO(a, LabelC(l))) +: linearize(body)

        case LetK(Label(name), _, contBody, body) =>
          conts += (name -> contBody)
          linearize(body)

        case LetF(List(), body) =>
          linearize(body)
        case LetF(FunDef(Label(name), _, _, funBody) :: rest, body) =>
          val asmFun = labeled(name, funPrelude(funBody) ++ linearize(funBody))
          linearize(LetF(rest, body)) ++ asmFun

        case AppK(Label(k), _) =>
          contOrJump(k)
        case AppK(Reg(I2), _) =>
          Seq(nl(RET))

        case AppF(Reg(fun), Label(rk), _) =>
          nl(CALL(fun)) +: contOrJump(rk)
        case AppF(Reg(fun), Reg(I2), _) =>
          Seq(nl(TCAL(fun)))

        case If(p, List(Reg(a), Reg(b)), Label(thenK), Label(elseK)) =>
          (conts remove thenK, conts remove elseK) match {
            case (Some(thenT), Some(elseT)) =>
              val elseP = labeled(elseK, linearize(elseT))
              val thenP = labeled(thenK, linearize(thenT))
              condJump(invertCond(p), a, b, elseK) +: (thenP ++ elseP)
            case (Some(thenElseT), None) =>
              val thenElseP = labeled(thenK, linearize(thenElseT))
              condJump(invertCond(p), a, b, elseK) +: thenElseP
            case (None, Some(elseThenT)) =>
              val elseThenP = labeled(elseK, linearize(elseThenT))
              condJump(p, a, b, thenK) +: elseThenP
            case (None, None) =>
              Seq(condJump(p, a, b, thenK), nl(JI(elseK)))
          }

        case Halt =>
          Seq(nl(HALT))
      }
    }

    funPrelude(tree) ++ linearize(tree)
  }
}
