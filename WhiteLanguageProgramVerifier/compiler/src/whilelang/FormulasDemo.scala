package whilelang
 
object FormulasDemo {
  def mainTest(args: Array[String]): Unit = {
    // You may not want to do the following import if names collide with your other classes       
    import Formulas._
                                                                        
    // These implicit conversions can be useful to avoid writing        
    // Var("a") or Const(42) for each variable and numerical constant.  
    implicit def numToConst(n: Int): Const = Const(n)
    implicit def strToVar(s: String): Var = Var(s)
                                                                        
    // checks whether 6 * 7 is always equal to 42. In other words,
    // checks that:                                                     
    //                                                                  
    //   !(a = 6 --> 7 * a = 42)                                        
    //                                                                  
    // ...is unsatisfiable.                                             
    val formula = Not(Implies(Equals("a", 6), Equals(Times(7, "a"), 42)))
 
    val result = isSat(formula) match {                                 
      case Some(false) => "unsat"
      case Some(true) => "sat"
      case None => "unknown"
    }
    
    println("The formula:")                                             
    // Prints the formula in SMT format.                                
    println(benchmarkString(formula))
    println("...has the status: " + result + ".")                       
  } 
}
