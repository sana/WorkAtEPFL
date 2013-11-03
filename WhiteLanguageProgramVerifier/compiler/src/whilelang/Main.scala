package whilelang

object Main {
  def main(args : Array[String]) : Unit = {
    if(args.length != 1) {
      Error("Please provide an input file.")
    }

    import java.io.{FileInputStream,IOException}

    val parser = new Parser

    try {
      val in = new FileInputStream(args(0))
      val parsed = parser.parseInputStream(in)

      println("Original program:")
      println("*****************\n")
      TreePrinter(parsed)
      println("*****************\n")

      val minimal = Conversion.convert(parsed)
      println("Minimal language program:")
      println("*****************\n")
      TreePrinter(minimal)
      println("*****************\n")

      val unrolled = Unrolling.unroll(minimal, 3)
      println("Unrolled program:")
      println("*****************\n")
      TreePrinter(unrolled)
      println("*****************\n")

      val formula = VCGen.compoVCG(unrolled)
      println("Formula for unrolled program:")
      println("*****************\n")
      println(formula);
      println("*****************\n")
      
      println("Z3 output:")
      println("*****************\n")
      Formulas.isSat(formula) match {
    	  case Some(true) => println("Program incorrect!")
    	  case Some(false) => println("Program correct!")
    	  case None => println("Error invoking Z3, please review the paramters and input") 
      }
      println("*****************\n")
    } catch {
      case e: IOException => Error(e.getMessage)
    }
  }
}
