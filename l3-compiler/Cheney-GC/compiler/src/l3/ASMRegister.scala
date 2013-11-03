package l3

object ASMRegisterBank extends Enumeration {
  val In, Out, Local = Value

  def prefix(b: Value): String = b match {
    case In => "I"
    case Out => "O"
    case Local => "L"
  }
}

/**
 * An L₃VM base register.
 */
case class ASMBaseRegister(bank: ASMRegisterBank.Value) {
  override def toString: String = ASMRegisterBank.prefix(bank) +"b"
}

/**
 * An L₃VM pseudo-register.
 */
case class ASMRegister(base: ASMBaseRegister, index: Int) {
  override def toString: String = ASMRegisterBank.prefix(base.bank) + index
}

/**
 * The full L₃VM register file.
 */
object ASMRegisterFile {
  val Ib = ASMBaseRegister(ASMRegisterBank.In)
  val Lb = ASMBaseRegister(ASMRegisterBank.Local)
  val Ob = ASMBaseRegister(ASMRegisterBank.Out)

  val in = for (i <- 0 until 16) yield ASMRegister(Ib, i)
  val out = for (i <- 0 until 16) yield ASMRegister(Ob, i)
  val local = for (i <- 0 until 224) yield ASMRegister(Lb, i)
}
