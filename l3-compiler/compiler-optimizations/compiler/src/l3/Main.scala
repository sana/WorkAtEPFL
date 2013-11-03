package l3

import l3.input.{ FileReader, SeqReader }
import scala.util.parsing.input.{ StreamReader }

object Main {
  def passThrough[T](f: T => Unit): T=>T = { t: T => f(t); t }

  def treePrinter[T <: Formatable](msg: String): T=>T =
    passThrough { tree =>
      val writer = new java.io.PrintWriter(System.out)
      println(msg)
      tree.toDocument.format(78, writer)
      writer.println()
      writer.flush()
    }

  def seqPrinter[T](msg: String): Seq[T]=>Seq[T] =
    passThrough { program =>
      println(msg)
      for (elem <- program)
        println(elem)
    }

  def main(args: Array[String]): Unit = {
    val inFiles = args.toList
    val inReader = SeqReader(inFiles map { FileReader(_) })

    L3Parser.program(new L3Scanner.Scanner(inReader)) match {
      case L3Parser.Success(program, _) =>
        val backEnd = (
          CL3NameAnalyzer
          andThen CL3ToCPSTranslator
          //andThen treePrinter("========== Before low level translation")
          andThen CPSDataRepresenter
          andThen treePrinter("========== After low level translation")
          andThen SymbolicCPSInterpreterLow
        )
        backEnd(program)
      case failure @ L3Parser.NoSuccess(_, _) =>
        Console.println(failure)
        exit(1)
    }
  }
}
