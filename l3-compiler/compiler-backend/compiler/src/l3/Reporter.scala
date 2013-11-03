package l3

import scala.util.parsing.input.Position

/**
 * Error-reporting module for the L₃ compiler.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object Reporter {
  def fatalError(pos: Position, msg: String): Nothing = {
    Console.println(pos.toString +": "+ msg)
    exit(1)
  }
}
