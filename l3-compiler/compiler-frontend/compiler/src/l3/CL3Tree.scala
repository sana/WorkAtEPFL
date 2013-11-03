package l3

import scala.util.parsing.input.Positional
import scala.text.Document
import Document._

/**
 * A module for CL₃ trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CL3TreeModule {
  type Name
  type Primitive

  private implicit def nameToDoc(n: Name): Document = text(n.toString)
  private implicit def primToDoc(p: Primitive): Document = text(p.toString)

  sealed trait Tree extends Formatable with Positional {
    def toDocument: Document = {
      import FormatHelpers._

      this match {
        case Let(bdgs, body) =>
          def bdgToDoc(b: (Name, Tree)): Document =
            paren(b._1 :/: b._2.toDocument)
          taggedParen2("let", pSeqToDoc(bdgs, bdgToDoc), body.toDocument)
        case LetRec(funs, body) =>
          taggedParen2("letrec",
                       pSeqToDoc(funs, (_: FunDef).toDocument),
                       body.toDocument)
        case If(c, t, e) =>
          taggedParen2("if", c.toDocument, t.toDocument :/: e.toDocument)
        case App(fun, args) =>
          pSeqToDoc(fun :: args, (_ : Tree).toDocument)
        case Prim(prim, args) =>
          taggedParen("@", prim :/: seqToDoc(args, (_ : Tree).toDocument))
        case Ident(name) =>
          nameToDoc(name)
        case Lit(l) =>
          l.toString
      }
    }
  }

  case class Let(bindings: List[(Name, Tree)], body: Tree) extends Tree
  case class LetRec(functions: List[FunDef], body: Tree) extends Tree
  case class If(cond: Tree, thenE: Tree, elseE: Tree) extends Tree
  case class App(fun: Tree, args: List[Tree]) extends Tree
  case class Prim(prim: Primitive, args: List[Tree]) extends Tree
  case class Ident(name: Name) extends Tree
  case class Lit(value: CL3Literal) extends Tree

  case class FunDef(name: Name, args: List[Name], body: Tree)
       extends Formatable with Positional {
         def toDocument: Document = {
           import FormatHelpers._
           val argsDoc = pSeqToDoc(args, nameToDoc)
           paren(name :/: taggedParen2("fun", argsDoc, body.toDocument))
         }
       }
}

/**
 * Module for trees after parsing: names and primitives are
 * represented as strings.
 */
object NominalCL3TreeModule extends CL3TreeModule {
  type Name = String
  type Primitive = String
}

/**
 * Module for trees after name analysis: names are represented as
 * symbols (globally-unique names) and primitives as objects.
 */
object SymbolicCL3TreeModule extends CL3TreeModule {
  type Name = Symbol
  type Primitive = L3Primitive
}
