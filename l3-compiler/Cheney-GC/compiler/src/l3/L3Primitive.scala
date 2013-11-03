package l3

/**
 * A class for L₃ primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

sealed abstract class L3Primitive(val name: String) {
  override def toString: String = name
  def arity: Int
}

trait Nullary extends L3Primitive { def arity = 0 }
trait Unary extends L3Primitive { def arity = 1 }
trait Binary extends L3Primitive { def arity = 2 }
trait Ternary extends L3Primitive { def arity = 3 }

sealed abstract class L3ValuePrimitive(name: String) extends L3Primitive(name)
sealed abstract class L3TestPrimitive(name: String) extends L3Primitive(name)

// Primitives on functions
case object L3FunctionP extends L3TestPrimitive("function?")
     with Unary

// Primitives on blocks
case class L3BlockAlloc(tag: Int) extends L3ValuePrimitive("block-alloc-"+ tag)
     with Unary
case object L3BlockP extends L3TestPrimitive("block?")
     with Unary
case object L3BlockTag extends L3ValuePrimitive("block-tag")
     with Unary
case object L3BlockLength extends L3ValuePrimitive("block-length")
     with Unary
case object L3BlockGet extends L3ValuePrimitive("block-get")
     with Binary
case object L3BlockSet extends L3ValuePrimitive("block-set!")
     with Ternary

// Primitives on integers
case object L3IntP extends L3TestPrimitive("int?")
     with Unary

case object L3IntAdd extends L3ValuePrimitive("+")
     with Binary
case object L3IntSub extends L3ValuePrimitive("-")
     with Binary
case object L3IntMul extends L3ValuePrimitive("*")
     with Binary
case object L3IntDiv extends L3ValuePrimitive("/")
     with Binary
case object L3IntMod extends L3ValuePrimitive("%")
     with Binary

case object L3IntLt extends L3TestPrimitive("<")
     with Binary
case object L3IntLe extends L3TestPrimitive("<=")
     with Binary
case object L3IntGe extends L3TestPrimitive(">=")
     with Binary
case object L3IntGt extends L3TestPrimitive(">")
     with Binary

case object L3CharRead extends L3ValuePrimitive("char-read")
     with Nullary
case object L3CharPrint extends L3ValuePrimitive("char-print")
     with Unary

// Primitives on booleans
case object L3BoolP extends L3TestPrimitive("bool?")
     with Unary

// Primitives on unit
case object L3UnitP extends L3TestPrimitive("unit?")
     with Unary

// Primitives on arbitrary values

case object L3Eq extends L3TestPrimitive("=")
     with Binary
case object L3Ne extends L3TestPrimitive("!=")
     with Binary

object L3Primitive {
  def isDefinedAt(name: String): Boolean =
    byName isDefinedAt name

  def isDefinedAt(name: String, arity: Int): Boolean =
    (byName isDefinedAt name) && (byName(name).arity == arity)

  def apply(name: String): L3Primitive =
    byName(name)

  private val blockAllocators = for (i <- 0 to 255) yield L3BlockAlloc(i)

  private val byName: Map[String, L3Primitive] =
    Map((Seq(L3FunctionP,
             L3BlockP, L3BlockTag, L3BlockLength, L3BlockGet, L3BlockSet,
             L3IntP, L3IntAdd, L3IntSub, L3IntMul, L3IntDiv, L3IntMod,
             L3IntLt, L3IntLe, L3Eq, L3Ne, L3IntGe, L3IntGt,
             L3CharRead, L3CharPrint,
             L3BoolP,
             L3UnitP) ++ blockAllocators)
        map { p => (p.name, p) } : _*)
}
