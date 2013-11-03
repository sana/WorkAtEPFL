package l3

/**
 * A class for symbols, i.e. globally-unique names.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

class Symbol(val name: String) {
  override def toString: String = name
}

object Symbol {
  private val counters = scala.collection.mutable.HashMap[String,Int]()

  private def freshName(prefix: String): String = {
    val count = counters.getOrElse(prefix, 1)
    counters.put(prefix, count + 1)
    prefix + "_" + count
  }

  def fresh(prefix: String): Symbol =
    new Symbol(freshName(prefix))
}
