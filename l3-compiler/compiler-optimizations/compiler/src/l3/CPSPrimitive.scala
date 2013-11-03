package l3

/**
 * A class for value-producing primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

sealed abstract class CPSValuePrimitive(val name: String) {
  override def toString: String = name
}

case object CPSAdd extends CPSValuePrimitive("+")
case object CPSSub extends CPSValuePrimitive("-")
case object CPSMul extends CPSValuePrimitive("*")
case object CPSDiv extends CPSValuePrimitive("/")
case object CPSMod extends CPSValuePrimitive("%")

case object CPSArithShiftL extends CPSValuePrimitive("<<")
case object CPSArithShiftR extends CPSValuePrimitive(">>")
case object CPSAnd extends CPSValuePrimitive("&")
case object CPSOr extends CPSValuePrimitive("|")

case object CPSCharRead extends CPSValuePrimitive("char-read")
case object CPSCharPrint extends CPSValuePrimitive("char-print")

case class CPSBlockAlloc(tag: Int) extends CPSValuePrimitive("block-alloc-"+tag)
case object CPSBlockTag extends CPSValuePrimitive("block-tag")
case object CPSBlockSize extends CPSValuePrimitive("block-size")
case object CPSBlockGet extends CPSValuePrimitive("block-get")
case object CPSBlockSet extends CPSValuePrimitive("block-set!")

case object CPSCopy extends CPSValuePrimitive("copy")

/**
 * A class for testing primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

sealed abstract class CPSTestPrimitive(val name: String) {
  override def toString: String = name
}

case object CPSLt extends CPSTestPrimitive("<")
case object CPSLe extends CPSTestPrimitive("<=")
case object CPSEq extends CPSTestPrimitive("=")
case object CPSNe extends CPSTestPrimitive("!=")
case object CPSGt extends CPSTestPrimitive(">")
case object CPSGe extends CPSTestPrimitive(">=")
