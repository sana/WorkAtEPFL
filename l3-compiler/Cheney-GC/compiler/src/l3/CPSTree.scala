package l3

import scala.text.Document
import Document._

/**
 * A module for CPS trees.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait CPSTreeModule {
  type Name
  type ValuePrimitive
  type TestPrimitive
  type Literal

  private implicit def nameToDoc(n: Name): Document = text(n.toString)

  sealed trait Tree extends Formatable {
    def toDocument: Document = {
      import FormatHelpers._

      def sugaredLetDocs(t: Tree): (List[Document], Document) = t match {
        case LetL(name, value, body) =>
          val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
          (paren(name :/: value.toString) :: bdgsDocs, bodyDoc)
        case LetP(name, prim, args, body) =>
          val argsDoc = seqToDoc(args, nameToDoc)
          val bdgsDoc = paren(name :/: taggedParen(prim.toString, argsDoc))
          val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
          (bdgsDoc :: bdgsDocs, bodyDoc)
        case LetK(name, args, contBody, body) =>
          val argsDoc = pSeqToDoc(args, nameToDoc)
          val contDoc = taggedParen2("cont", argsDoc, contBody.toDocument)
          val bdgsDoc = paren(name :/: contDoc)
          val (bdgsDocs, bodyDoc) = sugaredLetDocs(body)
          (bdgsDoc :: bdgsDocs, bodyDoc)
        case other =>
          (List(), other.toDocument)
      }

      this match {
        case LetL(_, _, _) | LetP(_, _, _, _) | LetK(_, _, _, _) =>
          val (bdgsDocs, bodyDoc) = sugaredLetDocs(this)
          val tag = if (bdgsDocs.length > 1) "let*" else "let"
          taggedParen2(tag, paren(foldDoc(bdgsDocs)), bodyDoc)
        case LetF(fns, body) =>
          taggedParen2("letf",
                       pSeqToDoc(fns, (_: FunDef).toDocument),
                       body.toDocument)
        case AppF(fun, retK, args) =>
          taggedParen(fun.toString, seqToDoc(retK :: args, nameToDoc))
        case AppK(cont, args) =>
          taggedParen(cont.toString, seqToDoc(args, nameToDoc))
        case If(p, args, thenK, elseK) =>
          taggedParen2("if",
                       paren(p.toString :/: seqToDoc(args, nameToDoc)),
                       thenK :/: elseK)
        case Halt =>
          "(halt)"
      }
    }

    /**
     * Produce a new version of the tree where all names have been
     * substituted according to the given partial function. Names
     * that are not in the partial function's domain are left
     * untouched.
     */
    def subst(partialS: PartialFunction[Name, Name]): Tree = {
      val s = { n: Name => if (partialS isDefinedAt n) partialS(n) else n }

      def substIn(t: Tree): Tree = t match {
        case LetL(name, value, body) =>
          LetL(s(name), value, substIn(body))
        case LetP(name, prim, args, body) =>
          LetP(s(name), prim, args map s, substIn(body))
        case LetK(name, args, contBody, body) =>
          LetK(s(name), args map s, substIn(contBody), substIn(body))
        case LetF(functions, body) =>
          val substFunctions = functions map {
            case FunDef(name, retK, args, body) =>
              FunDef(s(name), s(retK), args map s, substIn(body))
          }
          LetF(substFunctions, substIn(body))
        case AppK(cont, args) =>
          AppK(s(cont), args map s)
        case AppF(fun, retK, args) =>
          AppF(s(fun), s(retK), args map s)
        case If(cond, args, thenK, elseK) =>
          If(cond, args map s, s(thenK), s(elseK))
        case Halt =>
          Halt
      }

      substIn(this)
    }
  }

  case class LetL(name: Name, value: Literal, body: Tree) extends Tree
  case class LetP(name: Name, prim: ValuePrimitive, args: List[Name], body:Tree)
       extends Tree
  case class LetK(name: Name, args: List[Name], contBody: Tree, body: Tree)
       extends Tree
  case class LetF(functions: List[FunDef], body: Tree) extends Tree
  case class AppK(cont: Name, args: List[Name]) extends Tree
  case class AppF(fun: Name, retK: Name, args: List[Name]) extends Tree
  case class If(cond: TestPrimitive, args: List[Name], thenK: Name, elseK: Name)
       extends Tree
  case object Halt extends Tree

  case class FunDef(name: Name, retK: Name, args: List[Name], body: Tree)
       extends Formatable {
         def toDocument: Document = {
           import FormatHelpers._
           val argsDoc = pSeqToDoc(retK :: args, nameToDoc)
           paren(name :/: taggedParen2("fun", argsDoc, body.toDocument))
         }
       }
}

/**
 * Module for "high-level" CPS trees: the full L3 literals and
 * primitives are available.
 */
object SymbolicCPSTreeModule extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = L3ValuePrimitive
  type TestPrimitive = L3TestPrimitive
  type Literal = CL3Literal
}

/**
 * Module for "low-level" CPS trees: the only literal values are
 * integers, and the primitives work on integers and/or pointers to
 * heap-allocated blocks.
 */
object SymbolicCPSTreeModuleLow extends CPSTreeModule {
  type Name = Symbol
  type ValuePrimitive = CPSValuePrimitive
  type TestPrimitive = CPSTestPrimitive
  type Literal = Int
}

/**
 * Module for register-allocated CPS trees: names either represent ASM
 * registers or ASM labels. (Since register names are often reused,
 * names are no longer globally unique as previously).
 */
object RegisterCPSTreeModule extends CPSTreeModule {
  sealed abstract class Name {
    override def toString: String = this match {
      case Reg(r) => r.toString
      case Label(l) => l.toString
    }
  }
  case class Reg(reg: ASMRegister) extends Name
  case class Label(label: Symbol) extends Name

  type ValuePrimitive = CPSValuePrimitive
  type TestPrimitive = CPSTestPrimitive
  type Literal = Int
}
