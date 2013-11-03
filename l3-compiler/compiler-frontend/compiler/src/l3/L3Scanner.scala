package l3

import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * Lexical analyzer for the L₃ language.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object L3Scanner extends Scanners with L3Tokens {
  override def whitespace: Parser[Any] =
    rep(anyCharIn(" \t\n\r") | (';' ~ rep(anyCharNotIn("\n\r"))))

  override def token: Parser[Token] = (
      opt('-') ~ rep1(digit)
        ^^ { case s ~ ds => IntL(charsToString((s getOrElse '0') :: ds).toInt) }
    | identStart ~! rep(identStart | digit)
        ^^ { case l ~ ls => processIdent(charsToString(l :: ls)) }
    | '"' ~> rep(anyCharNotIn("\n\r\"" + EofCh)) <~ '"'
        ^^ { chars => StringL(charsToString(chars)) }
    | '\'' ~> anyCharNotIn(EofCh.toString) <~ '\''
        ^^ CharL
    | '(' ^^^ LPar
    | ')' ^^^ RPar
    | '@' ^^^ Prim
    | '#' ~ 't' ^^^ BooleanL(true)
    | '#' ~ 'f' ^^^ BooleanL(false)
    | '#' ~ 'u' ^^^ UnitL
    | EofCh ^^^ EOF
    | failure("illegal character"))

  private def digit: Parser[Char] =
    anyCharIn("0123456789")
  private def identStart: Parser[Char] =
    anyCharIn("abcdefghijklmnopqrstuvwxyz|!%&*+-./:<=>?^_~")

  private def anyCharIn(set: String): Parser[Char] =
    elem("any in {" + set + "}",
         { c: Char => (set indexOf c.toLower) != -1 })
  private def anyCharNotIn(set: String): Parser[Char] =
    elem("any not in {" + set + "}",
         { c: Char => (set indexOf c.toLower) == -1 })

  private def processIdent(s: String): Token = s match {
    case "and"    => And
    case "begin"  => Begin
    case "cond"   => Cond
    case "def"    => Def
    case "defrec" => DefRec
    case "fun"    => Fun
    case "if"     => If
    case "let"    => Let
    case "let*"   => Let_*
    case "letrec" => LetRec
    case "not"    => Not
    case "or"     => Or
    case "rec"    => Rec
    case ident    => Ident(ident)
  }

  private def charsToString(chars: List[Char]): String =
    chars mkString ""
}
