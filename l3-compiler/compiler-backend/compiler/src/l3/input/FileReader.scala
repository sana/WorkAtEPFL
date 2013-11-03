package l3.input

import java.io.{File, FileReader => JFileReader}
import scala.util.parsing.input.{Reader, StreamReader, Position}

class FilePosition(val fileName: String, pos: Position) extends Position {
  def line: Int = pos.line
  def column: Int = pos.column
  def lineContents: String = pos.longString.split("\n")(0) // HACK
  override def toString: String = fileName + ":" + super.toString
}

object FileReader {
  def apply(fileName: String): Reader[Char] =
    new FileReader(fileName, StreamReader(new JFileReader(new File(fileName))))
}

class FileReader(fileName: String, reader: Reader[Char]) extends Reader[Char] {
  def first: Char = reader.first
  def rest: Reader[Char] = new FileReader(fileName, reader.rest)
  def atEnd: Boolean = reader.atEnd

  def pos: Position = new FilePosition(fileName, reader.pos)
}
