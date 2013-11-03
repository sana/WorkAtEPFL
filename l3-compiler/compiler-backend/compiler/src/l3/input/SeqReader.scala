package l3.input

import scala.util.parsing.input.{Reader, Position}

object EmptyReader extends Reader[Nothing] {
  def first: Nothing = error("empty reader contains no element")
  def rest: Reader[Nothing] = this
  def pos: Position = error("empty reader has no position")
  def atEnd: Boolean = true
}

class SeqPosition(val index: Int, pos: Position) extends Position {
  def line: Int = pos.line
  def column: Int = pos.column
  def lineContents: String = pos.longString.split("\n")(0) // HACK
  override def <(that: Position) = that match {
    case thatSeqPos: SeqPosition =>
      index < thatSeqPos.index || (index == thatSeqPos.index && super.<(that))
    case _ =>
      error("comparing a SeqPosition with another kind of position")
  }
  override def toString: String = pos.toString
}

class SeqElemReader[+T](index: Int, r: Reader[T]) extends Reader[T] {
  def first: T = r.first
  def rest: Reader[T] = new SeqElemReader(index, r.rest)
  def pos: Position = new SeqPosition(index, r.pos)
  def atEnd: Boolean = r.atEnd
}

object SeqReader {
  def apply[T](rs: List[Reader[T]]): Reader[T] = {
    val seqRs = rs.zipWithIndex.map { case (r, i) => new SeqElemReader(i, r) }
    seqRs.foldLeft(EmptyReader : Reader[T])(new SeqReader(_, _))
  }
}

class SeqReader[+T](r1: Reader[T], r2: Reader[T]) extends Reader[T] {
  def first: T = if (r1.atEnd) r2.first else r1.first
  def rest: Reader[T] = if (r1.atEnd) r2.rest else new SeqReader(r1.rest, r2)
  def pos: Position = if (r1.atEnd) r2.pos else r1.pos
  def atEnd: Boolean = r1.atEnd && r2.atEnd
}
