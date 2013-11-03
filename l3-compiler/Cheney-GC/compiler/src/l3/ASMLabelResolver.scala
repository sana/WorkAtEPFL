package l3

import collection.mutable.{ Map => MutableMap }

import BitTwiddling.signExtend18
import l3.{ LabeledASMInstructionModule => L }
import l3.{ PCRelativeASMInstructionModule => R }

/**
 * Label resolution for the ASM language. Translates a program in
 * which addresses are represented as symbolic labels to one where
 * they are represented as PC-relative (or absolute, in some cases)
 * addresses.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object ASMLabelResolver extends (L.LabeledProgram => R.Program) {
  def apply(labeledProgram: L.LabeledProgram): R.Program =
    resolve(labeledProgram)

  def resolve(program: L.LabeledProgram): R.Program = {
    val indexedProgram = program.zipWithIndex
    val labelAddr = Map(
      (for { (L.LabeledInstruction(labels, _), a) <- indexedProgram
             l <- labels }
         yield (l, a)) : _*)

    for ((labeledInstr, addr) <- indexedProgram) yield {
      def delta(l: L.Label): Int = labelAddr(l) - addr
      labeledInstr.instruction match {
        case L.ADD(a, b, c)       => R.ADD(a, b, c)
        case L.SUB(a, b, c)       => R.SUB(a, b, c)
        case L.MUL(a, b, c)       => R.MUL(a, b, c)
        case L.DIV(a, b, c)       => R.DIV(a, b, c)
        case L.MOD(a, b, c)       => R.MOD(a, b, c)
        case L.ASL(a, b, c)       => R.ASL(a, b, c)
        case L.ASR(a, b, c)       => R.ASR(a, b, c)
        case L.AND(a, b, c)       => R.AND(a, b, c)
        case L.OR(a, b, c)        => R.OR(a, b, c)
        case L.JLT(a, b, l)       => R.JLT(a, b, delta(l))
        case L.JLE(a, b, l)       => R.JLE(a, b, delta(l))
        case L.JEQ(a, b, l)       => R.JEQ(a, b, delta(l))
        case L.JNE(a, b, l)       => R.JNE(a, b, delta(l))
        case L.JGE(a, b, l)       => R.JGE(a, b, delta(l))
        case L.JGT(a, b, l)       => R.JGT(a, b, delta(l))
        case L.JI(l)              => R.JI(delta(l))
        case L.TCAL(a)            => R.TCAL(a)
        case L.CALL(a)            => R.CALL(a)
        case L.RET                => R.RET
        case L.HALT               => R.HALT
        case L.LDLO(a, L.IntC(s)) => R.LDLO(a, s)
        case L.LDHI(a, u)         => R.LDHI(a, u)
        case L.MOVE(a, b)         => R.MOVE(a, b)
        case L.RALO(a, s)         => R.RALO(a, s)
        case L.BALO(a, b, t)      => R.BALO(a, b, t)
        case L.BSIZ(a, b)         => R.BSIZ(a, b)
        case L.BTAG(a, b)         => R.BTAG(a, b)
        case L.BGET(a, b, c)      => R.BGET(a, b, c)
        case L.BSET(a, b, c)      => R.BSET(a, b, c)
        case L.CREA(a)            => R.CREA(a)
        case L.CPRI(a)            => R.CPRI(a)

        case L.LDLO(a, L.LabelC(l)) =>
          val addr = labelAddr(l) << 2
          assume(signExtend18(addr & 0x3FFFF) == addr)
          R.LDLO(a, addr)
      }
    }
  }
}
