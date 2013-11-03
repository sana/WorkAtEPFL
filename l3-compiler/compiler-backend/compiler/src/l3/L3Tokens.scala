package l3

import scala.util.parsing.syntax.Tokens

/**
 * Tokens for the L₃ language, used by the lexical analyzer (a.k.a.
 * the scanner).
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */
trait L3Tokens extends Tokens {
  abstract class L3Token extends Token {
    override def toString: String = chars
  }

  case object LPar extends L3Token { def chars = "(" }
  case object RPar extends L3Token { def chars = ")" }
  case object Def extends L3Token { def chars = "def" }
  case object DefRec extends L3Token { def chars = "defrec" }
  case object Fun extends L3Token { def chars = "fun" }
  case object Let extends L3Token { def chars = "let" }
  case object Let_* extends L3Token { def chars = "let*" }
  case object LetRec extends L3Token { def chars = "letrec" }
  case object Rec extends L3Token { def chars = "rec" }
  case object Begin extends L3Token { def chars = "begin" }
  case object Cond extends L3Token { def chars = "cond" }
  case object If extends L3Token { def chars = "if" }
  case object And extends L3Token { def chars = "and" }
  case object Or extends L3Token { def chars = "or" }
  case object Not extends L3Token { def chars = "not" }
  case object Prim extends L3Token { def chars = "@" }
  case class Ident(val chars: String) extends L3Token
  case class IntL(n: Int) extends L3Token { def chars = n.toString }
  case class StringL(s: String) extends L3Token { def chars = '"'+ s +'"' }
  case class CharL(c: Char) extends L3Token { def chars = "'"+ c +"'" }
  case class BooleanL(v: Boolean) extends L3Token {
    def chars = if (v) "#t" else "#f"
  }
  case object UnitL extends L3Token { def chars = "#u" }
}
