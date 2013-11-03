package l3

import scala.util.parsing.combinator.syntactical.TokenParsers
import NominalCL3TreeModule._

/**
 * Syntactical analyzer for the L₃ language. Produces desugared CL₃
 * trees.
 *
 * @author Michel Schinz
 */
object L3Parser extends TokenParsers {
  type Tokens = L3Tokens
  val lexical = L3Scanner

  def program: Parser[Tree] =
    phrase(topLevelExpr)

  private def topLevelExpr: Parser[Tree] =
    defProgram | defrecProgram | exprProgram
  private def defProgram: Parser[Tree] = positioned(
    paren(lexical.Def ~> identStr ~ expr) ~ topLevelExpr
      ^^ { case (i ~ e) ~ p => Let(List((i, e)), p) })
  private def defrecProgram: Parser[Tree] = positioned(
    paren(lexical.DefRec ~> identStr ~ anonFun) ~ topLevelExpr
      ^^ { case (i ~ f) ~ p => LetRec(List(FunDef(i, f._1, f._2)), p) })
  private def exprProgram: Parser[Tree] = positioned(
    expr ~ (topLevelExpr ?)
      ^^ { case e ~ Some(p) => sBegin(List(e, p))
           case e ~ None => e })

  private def expr: Parser[Tree] =
    (fun | let | let_* | letrec | rec | begin | cond | if_ | and | or | not
     | app | prim | ident | int | string | char | boolean | unit)
  private def exprs: Parser[List[Tree]] =
    rep1(expr)
  private def fun: Parser[Tree] = positioned(
    anonFun
      ^^ { case f => sFun(f._1, f._2) })
  private def let: Parser[Tree] = positioned(
    paren(lexical.Let ~> paren(rep(binding)) ~ exprs)
      ^^ { case bdgs ~ body => Let(bdgs, sBegin(body)) })
  private def let_* : Parser[Tree] = positioned(
    paren(lexical.Let_* ~> paren(rep(binding)) ~ exprs)
      ^^ { case bdgs ~ body => sLet_*(bdgs, sBegin(body)) })
  private def letrec: Parser[Tree] = positioned(
    paren(lexical.LetRec ~> paren(rep(funDef)) ~ exprs)
      ^^ { case funs ~ body => LetRec(funs, sBegin(body))  })
  private def rec: Parser[Tree] = positioned(
    paren(lexical.Rec ~> identStr ~ paren(rep(binding)) ~ exprs)
      ^^ { case name ~ bdgs ~ body => sRec(name, bdgs, sBegin(body)) })
  private def begin: Parser[Tree] = positioned(
    paren(lexical.Begin ~> exprs)
      ^^ { case exprs => sBegin(exprs) })
  private def cond: Parser[Tree] = positioned(
    paren(lexical.Cond ~> rep1(paren(expr ~ expr)))
      ^^ { case alts => sCond(alts map { case e1 ~ e2 => (e1, e2) }) })
  private def if_ : Parser[Tree] = positioned(
    paren(lexical.If ~> expr ~ expr ~ (expr ?))
      ^^ { case c ~ t ~ e => If(c, t, e getOrElse Lit(UnitLit)) })
  private def and: Parser[Tree] = positioned(
    paren(lexical.And ~> expr ~ expr)
      ^^ { case e1 ~ e2 => sAnd(e1, e2) })
  private def or: Parser[Tree] = positioned(
    paren(lexical.Or ~> expr ~ expr)
      ^^ { case e1 ~ e2 => sOr(e1, e2) })
  private def not: Parser[Tree] = positioned(
    paren(lexical.Not ~> expr)
      ^^ { case e => sNot(e) })
  private def app: Parser[Tree] = positioned(
    paren(expr ~ rep(expr))
      ^^ { case f ~ as => App(f, as) })
  private def prim: Parser[Tree] = positioned(
    paren(lexical.Prim ~> identStr ~ rep(expr))
      ^^ { case p ~ as => Prim(p, as) })

  private def binding: Parser[(String,Tree)] =
    (paren(identStr ~ expr)
      ^^ { case ident ~ expr => (ident, expr) })
  private def funDef: Parser[FunDef] =
    (paren(identStr ~ anonFun)
      ^^ { case ident ~ f => FunDef(ident, f._1, f._2) })

  private def anonFun: Parser[(List[String], Tree)] =
    (paren(lexical.Fun ~> paren(rep(identStr)) ~ exprs)
      ^^ { case idents ~ body => (idents, sBegin(body)) })

  private def identStr: Parser[String] = (
    elem("identifier", _.isInstanceOf[lexical.Ident])
      ^^ { case lexical.Ident(n) => n })
  private def ident: Parser[Ident] =
    positioned(identStr ^^ Ident)
  private def int: Parser[Tree] = positioned(
    elem("integer", _.isInstanceOf[lexical.IntL])
      ^^ { case lexical.IntL(n) => Lit(IntLit(n)) })
  private def string: Parser[Tree] = positioned(
    elem("string", _.isInstanceOf[lexical.StringL])
      ^^ { case lexical.StringL(s) => sStringLit(s) })
  private def char: Parser[Tree] = positioned(
    elem("character", _.isInstanceOf[lexical.CharL])
      ^^ { case lexical.CharL(c) => sCharLit(c) })
  private def boolean: Parser[Tree] = positioned(
    elem("boolean", _.isInstanceOf[lexical.BooleanL])
      ^^ { case lexical.BooleanL(v) => Lit(BooleanLit(v)) })
  private def unit: Parser[Tree] = positioned(
    elem("unit", _ == lexical.UnitL)
      ^^^ { Lit(UnitLit) })

  private def paren[T](p: Parser[T]): Parser[T] =
    lexical.LPar ~> p <~ lexical.RPar

  // Fresh name generation

  private var freshCounter = 0
  private def freshName(prefix: String): String = {
    freshCounter += 1
    prefix + "$" + freshCounter
  }

  // Syntactic sugar translation.

  private def sFun(args: List[String], body: Tree): Tree = {
    val fName = freshName("fun")
    LetRec(List(FunDef(fName, args, body)), Ident(fName))
  }
  private def sLet_*(bdgs: List[(String,Tree)], body: Tree): Tree =
    (bdgs :\ body)( (b, t) => Let(List(b), t))
  private def sBegin(exprs: List[Tree]): Tree =
    exprs reduceRight { (e1, e2) => Let(List((freshName("begin"), e1)), e2) }
  private def sRec(name: String, bdgs: List[(String, Tree)], body: Tree) =
    LetRec(List(FunDef(name, bdgs map { _._1 }, body)),
           App(Ident(name), bdgs map { _._2 }))
  private def sAnd(e1: Tree, e2: Tree): Tree =
    If(e1, e2, Lit(BooleanLit(false)))
  private def sOr(e1: Tree, e2: Tree): Tree = {
    val v1Name = freshName("or")
    Let(List((v1Name, e1)), If(Ident(v1Name), Ident(v1Name), e2))
  }
  private def sNot(e: Tree): Tree =
    If(e, Lit(BooleanLit(false)), Lit(BooleanLit(true)))
  private def sCond(branches: List[(Tree, Tree)]): Tree =
    (branches :\ (Lit(UnitLit) : Tree)){ case ((c, t), e) => If(c, t, e) }
  private def sStringLit(s: String): Tree = {
    val blockName = freshName("string")
    Let(List((blockName, Prim("block-alloc-"+ BlockTag.String.id,
                              List(Lit(IntLit(s.length)))))),
        sBegin((s.toList.zipWithIndex map { case (c, i) =>
          Prim("block-set!",
               List(Ident(blockName), Lit(IntLit(i)), sCharLit(c))) })
               :+ Ident(blockName)))
  }
  private def sCharLit(c: Char): Tree =
    Lit(IntLit(c.toInt))
}
