package whilelang

object Error {
  def apply(msg: String): Nothing = {
    Console.err.println(msg)
    println("The program terminated with an error.")
    exit(-1)
  }
}
