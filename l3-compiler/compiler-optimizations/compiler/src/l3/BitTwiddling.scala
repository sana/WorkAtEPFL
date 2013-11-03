package l3

/**
 * Bit twiddling utility module.
 * Parts taken from http://graphics.stanford.edu/~seander/bithacks.html
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object BitTwiddling {
  def bitsToIntMSBF(bits: List[Int]): Int =
    (0 /: bits){ (v, b) => (v << 1) | b }

  def signExtend18(value: Int): Int = {
    val m = 1 << (18 - 1)
    (value ^ m) - m
  }
}
