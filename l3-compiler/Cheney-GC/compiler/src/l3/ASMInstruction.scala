package l3

import collection.Set

/**
 * A module for ASM instructions.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait ASMInstructionModule {
  type Label
  type Constant

  sealed abstract class Instruction

  case class ADD(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class SUB(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class MUL(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class DIV(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class MOD(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction

  case class ASL(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class ASR(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class AND(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class OR(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction

  case class JLT(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JLE(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JEQ(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JNE(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JGE(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JGT(a: ASMRegister, b: ASMRegister, d: Label) extends Instruction
  case class JI(d: Label) extends Instruction

  case class TCAL(r: ASMRegister) extends Instruction
  case class CALL(r: ASMRegister) extends Instruction
  case object RET extends Instruction
  case object HALT extends Instruction

  case class LDLO(a: ASMRegister, s: Constant) extends Instruction
  case class LDHI(a: ASMRegister, u: Int) extends Instruction
  case class MOVE(a: ASMRegister, b: ASMRegister) extends Instruction

  case class RALO(a: ASMBaseRegister, s: Int) extends Instruction
  case class BALO(a: ASMRegister, b: ASMRegister, t: Int) extends Instruction
  case class BSIZ(a: ASMRegister, b: ASMRegister) extends Instruction
  case class BTAG(a: ASMRegister, b: ASMRegister) extends Instruction
  case class BGET(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction
  case class BSET(a: ASMRegister, b: ASMRegister, c: ASMRegister)
       extends Instruction

  case class CREA(a: ASMRegister) extends Instruction
  case class CPRI(a: ASMRegister) extends Instruction

  sealed case class LabeledInstruction(labels: Set[Label],
                                       instruction: Instruction) {
    override def toString: String =
      (labels map { _.toString + ":\n" } mkString) + "        " + instruction
  }

  type Program = Seq[Instruction]
  type LabeledProgram = Seq[LabeledInstruction]
}

/**
 * A module for ASM instructions labeled explicitly by a symbol.
 */
object LabeledASMInstructionModule extends ASMInstructionModule {
  type Label = Symbol
  abstract class Constant
  case class LabelC(l: Label) extends Constant
  case class IntC(v: Int) extends Constant
}

/**
 * A module for ASM instructions labeled implicitly by their
 * position.
 */
object PCRelativeASMInstructionModule extends ASMInstructionModule {
  type Label = Int
  type Constant = Int
}
