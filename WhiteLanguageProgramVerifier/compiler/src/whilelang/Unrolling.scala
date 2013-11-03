package whilelang

import scala.collection.mutable.ListBuffer

object Unrolling {
  /** Unrolls all loops in a program in the guarded command language 'factor' times, and returns the resulting
   * loop-free program. */
  def unrollListCond(list: List[List[Command]], cond: BoolExpr): List[Command] = {
    list match {
      case List() => List(Assume(Negation(cond)), Skip())
      case x :: xs => List(Choice(Block(x), Block(unrollListCond(xs, cond))))
    }
  }

  def unrollList(list: List[List[Command]]): List[Command] = {
    list match {
      case List() => List(Skip())
      case x :: xs => List(Choice(Block(x), Block(unrollList(xs))))
    }
  }

  def unroll(prog: Command, factor: Int): Command =
    prog match {
      case Assignment(v, t) => prog
      case Block(coms) => Block(coms.map( com => unroll(com, factor) ))
      
      /* We got rid of this in the conversion phase
      case WhileLoop(cond, com) => {
        var cmd: Command = unroll(com, factor)
        var list = new ListBuffer[List[Command]]()
        for (i <- 1 to factor)
        {
          var current_list = new ListBuffer[Command]()
          for (j <- 1 to i)
          {
            current_list += Assume(cond)
            current_list += cmd
          }
          list += current_list.toList
        }
        unrollListCond(list.toList, cond).head
      }
      */
      
      case Skip() => prog
      case Assume(expr) => prog
      case Assert(expr) => prog
      case Choice(choice1, choice2) => Choice(unroll(choice1, factor), unroll(choice2, factor))
      case Havoc(v) => prog
      case Loop(command) => {
        var cmd: Command = unroll(command, factor)
        var list = new ListBuffer[Command]()
        for (i <- 1 to factor)
        {
          list += cmd
        }
        Block(list.toList)
      }

      case _ => prog
  }

}
