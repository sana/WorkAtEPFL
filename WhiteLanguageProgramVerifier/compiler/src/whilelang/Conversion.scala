package whilelang

import scala.collection.mutable.ListBuffer

object Conversion {
  // Converts a program into an equivalent program in the guarded command language (using only sequential composition, havoc, assume, assert, [], and loop)

  def convert(prog: Command): Command = {
    prog match {
      case Assignment(v, t) => prog
      case IfThenElse(cond, ifcom, elsecom) => Choice(
        Block(List(Assume(cond), convert(ifcom))),
        Block(List(Assume(Negation(cond)), convert(elsecom)))
      )
      case Block(coms) => Block(coms.map( com => convert(com) ))
      case WhileLoop(cond, com) => {
        Block(List(Loop(Block(List(Assume(cond), convert(com)))), Assume(Negation(cond))))
    	//WhileLoop(cond, convert(com))
      }
      case Skip() => prog
      case Assume(expr) => prog
      case Assert(expr) => prog
      case Choice(choice1, choice2) => Choice(convert(choice1), convert(choice2))
      case Havoc(v) => prog
      case Loop(command) => Loop(convert(command))
    }
  }
}

