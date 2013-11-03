package l3

/**
 * Literal values for the CL₃ language.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

sealed trait CL3Literal {
  override def toString: String = this match {
    case IntLit(i) => i.toString
    case BooleanLit(v) => if (v) "#t" else "#f"
    case UnitLit => "#u"
  }
}

case class IntLit(value: Int) extends CL3Literal
case class BooleanLit(value: Boolean) extends CL3Literal
case object UnitLit extends CL3Literal
