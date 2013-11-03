package l3

import scala.text.Document
import Document._

/**
 * A trait for objects that can pretty-print themselves as
 * scala.text.Document instances.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

trait Formatable {
  def toDocument: Document
}

object FormatHelpers {
  def paren(d: Document): Document =
    group("(" :: nest(1, d) :: ")")

  def taggedParen(tag: String, d: Document): Document =
    group("(" :: tag :: " " :: nest(1 + tag.length + 1, d) :: ")")

  def taggedParen2(tag: String, d1: Document, d2: Document) =
    taggedParen(tag, d1 :: nest(-tag.length, break :: d2))

  def foldDoc(docs: Seq[Document]): Document = docs match {
    case Nil => empty
    case d :: Nil => d
    case d :: ds => d :/: foldDoc(ds)
  }

  def seqToDoc[T](docs: Seq[T], toDoc: T=>Document): Document =
    foldDoc(docs map toDoc)

  def pSeqToDoc[T](docs: Seq[T], toDoc: T => Document): Document =
    paren(seqToDoc(docs, toDoc))

  implicit def strToDoc(s: String): Document = text(s)
  implicit def intToDoc(i: Int): Document = text(i.toString)
}
