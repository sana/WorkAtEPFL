package l3

/**
 * Helper module for IO functions in L₃ and intermediate languages.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object IO {
  def readChar(): Int =
    System.in.read()

  def printChar(c: Int): Unit = {
    System.out.print(c.toChar)
    System.out.flush()
  }
}
