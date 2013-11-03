package l3

import java.io.File
import java.util.Formatter
import PCRelativeASMInstructionModule._

/**
 * Assembly program writer. Dumps a program to a textual file, in
 * which each line is composed of an encoded instruction represented
 * as a 32-bit hexadecimal value, followed by a textual representation
 * of the instruction.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

class ASMFileWriter(fileName: String) extends (Program => Unit) {
  def apply(program: Program): Unit = {
    val formatter = new Formatter(new File(fileName))
    for (instr <- program)
      formatter.format("%08x  %s\n", int2Integer(encode(instr)), instr)
    formatter.close()
  }

  private object Opcode extends Enumeration {
    val ADD, SUB, MUL, DIV, MOD = Value
    val ASL, ASR, AND, OR = Value
    val JLT, JLE, JEQ, JNE, JGE, JGT, JI = Value
    val TCAL, CALL, RET, HALT = Value
    val LDLO, LDHI, MOVE = Value
    val RALO, BALO, BSIZ, BTAG, BGET, BSET = Value
    val CREA, CPRI = Value
  }

  private def encode(instr: Instruction): Int = instr match {
    case ADD(a, b, c) => packRRR(Opcode.ADD, a, b, c)
    case SUB(a, b, c) => packRRR(Opcode.SUB, a, b, c)
    case MUL(a, b, c) => packRRR(Opcode.MUL, a, b, c)
    case DIV(a, b, c) => packRRR(Opcode.DIV, a, b, c)
    case MOD(a, b, c) => packRRR(Opcode.MOD, a, b, c)

    case ASL(a, b, c) => packRRR(Opcode.ASL, a, b, c)
    case ASR(a, b, c) => packRRR(Opcode.ASR, a, b, c)
    case AND(a, b, c) => packRRR(Opcode.AND, a, b, c)
    case OR(a, b, c) => packRRR(Opcode.OR, a, b, c)

    case JLT(a, b, d) => packRRD(Opcode.JLT, a, b, d)
    case JLE(a, b, d) => packRRD(Opcode.JLE, a, b, d)
    case JEQ(a, b, d) => packRRD(Opcode.JEQ, a, b, d)
    case JNE(a, b, d) => packRRD(Opcode.JNE, a, b, d)
    case JGE(a, b, d) => packRRD(Opcode.JGE, a, b, d)
    case JGT(a, b, d) => packRRD(Opcode.JGT, a, b, d)
    case JI(d) => pack(encOp(Opcode.JI), encSInt(d, 26))

    case TCAL(r) => packR(Opcode.TCAL, r)
    case CALL(r) => packR(Opcode.CALL, r)
    case RET => pack(encOp(Opcode.RET), pad(26))
    case HALT => pack(encOp(Opcode.HALT), pad(26))

    case LDLO(a, s) => pack(encOp(Opcode.LDLO), encReg(a), encSInt(s, 18))
    case LDHI(a, u) =>
      pack(encOp(Opcode.LDHI), encReg(a), pad(2), encUInt(u, 16))
    case MOVE(a, b) => packRR(Opcode.MOVE, a, b)

    case RALO(a, s) =>
      pack(encOp(Opcode.RALO), encBaseReg(a), encUInt(s, 8), pad(16))
    case BALO(a, b, t) =>
      pack(encOp(Opcode.BALO), encReg(a), encReg(b), encUInt(t, 8), pad(2))
    case BSIZ(a, b) => packRR(Opcode.BSIZ, a, b)
    case BTAG(a, b) => packRR(Opcode.BTAG, a, b)
    case BGET(a, b, c) => packRRR(Opcode.BGET, a, b, c)
    case BSET(a, b, c) => packRRR(Opcode.BSET, a, b, c)

    case CREA(a) => packR(Opcode.CREA, a)
    case CPRI(a) => packR(Opcode.CPRI, a)
  }

  private type BitField = (Int, Int)

  private def packR(opcode: Opcode.Value, a: ASMRegister): Int =
    pack(encOp(opcode), encReg(a), pad(18))

  private def packRR(opcode: Opcode.Value, a: ASMRegister, b: ASMRegister): Int=
    pack(encOp(opcode), encReg(a), encReg(b), pad(10))

  private def packRRR(opcode: Opcode.Value, a: ASMRegister, b: ASMRegister, c: ASMRegister): Int =
    pack(encOp(opcode), encReg(a), encReg(b), encReg(c), pad(2))

  private def packRRD(opcode: Opcode.Value, a: ASMRegister, b: ASMRegister, d: Int): Int =
    pack(encOp(opcode), encReg(a), encReg(b), encSInt(d, 10))

  private def encOp(opcode: Opcode.Value): BitField =
    encUInt(opcode.id, 6)

  private def encBaseReg(r: ASMBaseRegister): BitField = r match {
    case ASMRegisterFile.Lb => encUInt(0, 2)
    case ASMRegisterFile.Ib => encUInt(1, 2)
    case ASMRegisterFile.Ob => encUInt(2, 2)
  }

  private def encReg(r: ASMRegister): (Int, Int) = r match {
    case ASMRegister(ASMRegisterFile.Lb, i) => encUInt(i, 8)
    case ASMRegister(ASMRegisterFile.Ib, i) => encUInt(224 + i, 8)
    case ASMRegister(ASMRegisterFile.Ob, i) => encUInt(240 + i, 8)
  }

  private def encUInt(i: Int, len: Int): BitField = {
    assume(0 <= i && i < (1 << len))
    (i, len)
  }

  private def encSInt(i: Int, len: Int): BitField = {
    assume(-(1 << (len - 1)) <= i && i < (1 << (len - 1)), "i: "+ i)
    (i & ((1 << len) - 1), len)
  }

  private def pad(len: Int): BitField =
    encUInt(0, len)

  private def pack(values: BitField*): Int = {
    var packed: Int = 0
    for ((value, length) <- values)
      packed = (packed << length) | value
    packed
  }
}

object ASMFileWriter {
  def apply(fileName: String): ASMFileWriter =
    new ASMFileWriter(fileName)
}
