package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

import scala.collection.mutable.HashMap

/** This object implements a parser for Featherweight Java
 *  check <a href="http://citeseer.ist.psu.edu/igarashi99featherweight.html">
 *  the paper</a>.
 */
object FJ extends StandardTokenParsers   {

  lexical.delimiters ++= List(".", "{", "}", ",", "=", ";", "(", ")")
  lexical.reserved   ++= List("class", "extends", "super", "return", "new")
  
  
  /** <pre>
   *  Program     ::= { ClassDef } Expr
   * </pre>
   */
  def Prog: Parser[Tree] = positioned(
      rep(ClsDef) ~ Expr                          ^^ {case classes ~ main => Program(classes,main)}
  )
            
  /** <pre>
   *  ClassDef ::= "class" C extends C "{" 
   *                   { FieldDef } ConstructorDef  { MethodDef } 
   *               "}"
   *  </pre>
   */
  def ClsDef: Parser[ClassDef] = positioned(
      "class" ~ ident ~ "extends" ~ ident ~ "{" ~
        rep(FldDef) ~ CtorDef ~ rep(MethDef) ~ 
      "}"                                         
         ^^ { case "class" ~ name ~ "extends" ~ superc ~ "{" ~ fields ~ ctor ~ methods ~ "}"=> ClassDef(name,superc,fields,ctor,methods)}
    | failure("illegal start of class definition")
  )

  /** <pre>
   *  FieldDef       ::= ident ident ";"
   *  </pre>
   */
  def FldDef: Parser[FieldDef] = positioned(
	ident ~ ident <~ ";"                       ^^ { case tpe ~ name => FieldDef(tpe,name)}
    | failure("illegal start of field definition")
  )

  /** <pre>
   *  ConstructorDef ::= ident "(" ArgList ")" "{"
   *                       "super" "(" Arguments ")" ";"
   *                       { ident "." ident "=" ident }
   *                     "}"
   *  </pre>
   */
  def CtorDef: Parser[CtrDef] = positioned(      
      ident ~ "(" ~ ParamList ~ ")" ~ "{" ~
        "super" ~ "(" ~ VarList ~ ")" ~ ";" ~
        rep(Init) ~
      "}"  ^^ {case name ~ "(" ~ params ~ ")" ~ "{" ~ "super" ~ "(" ~ supArgs ~ ")" ~ ";" ~ seq ~ "}" => CtrDef(name,params,supArgs,seq)}
    | failure("illegal start of constructor")
  )

  /**
   * Initializer ::= ident "." ident "=" ident ";"
   */
  def Init: Parser[Assign] = positioned(
      ident ~ "." ~ ident ~ "=" ~ ident <~ ";" ^^ {case obj ~ "." ~ field ~ "=" ~ value => Assign(obj, field, Var(value))}
  )
    
  /** <pre>
   *  ParamList ::= ident ident { "," ident ident }
   *            | empty
   *  </pre>
   */
  def ParamList: Parser[List[FieldDef]] = (
        ident ~ ident ~ rep("," ~> ident ~ ident  ^^ {case tpe ~ name => FieldDef(tpe,name)}) ^^ {case tpe ~ name ~ rest => FieldDef(tpe,name) :: rest}
      | success(Nil)
  )
      
  /** <pre>
   *  VarList ::= ident ident { "," ident ident }
   *            | epsilon
   *  </pre>
   */
  def VarList: Parser[List[Var]] = (
        ident ~ rep("," ~> ident ^^ {x => Var(x)})   ^^  { case name ~ rest => Var(name) :: rest }
      | success(Nil)
  )
  
  /** <pre>
   *  MethodDef ::= ident ident "(" Params ")" "{" "return" Expr ";" "}"
   *  </pre>
   */
  def MethDef: Parser[MethodDef] = positioned(
        ident ~ ident ~ "(" ~ ParamList ~ ")" ~ "{" ~ "return" ~ Expr ~ ";" ~ "}" 
          ^^  {case tpe ~ name ~ "(" ~ params ~ ")" ~ "{" ~ "return" ~ body ~ ";" ~ "}" => MethodDef(tpe,name,params,body)}
      | failure("illegal start of method definition")
  )
      
      
  /** <pre>
   *  Expressions ::= [Expr { "," Expr } ]
   *  </pre>
   */
  def Expressions: Parser[List[Expr]] = (
        Expr ~ rep("," ~> Expr)  ^^ {case expr ~ rest => expr :: rest }
      | success(Nil)
  )
   
   
  /** <pre>
   *  Expr ::= SimpleExpr {"." ident ["(" Expressions ")"]}
   *  </pre>
   */
  def Expr: Parser[Expr] = positioned(
        SimpleExpr ~ rep("." ~> ident ~ opt("(" ~> Expressions <~ ")")) ^^ {case se ~ xs => mkExpression(se,xs)}
  )
      
  /** <pre>
   *  SimpleExpr ::= ident
   *               | "new" C "(" Expressions ")"
   *               | "(" C ")" Expr
   *               | "(" Expr ")"
   *  </pre>
   */
  def SimpleExpr: Parser[Expr] = positioned(
        ident                                        ^^ {x => Var(x)}
      | "new" ~ ident ~ "(" ~ Expressions ~ ")"      ^^ {case "new" ~ name ~ "(" ~ args ~ ")" => New(name,args)}
      | "(" ~ ident ~ ")" ~ Expr                    ^^ {case "(" ~ tpe ~ ")" ~ obj => Cast(tpe,obj)}
      | "(" ~> Expr <~ ")"
      | failure("illegal start of simple expression")
      )
      
  type Output = Tree
  
  def mkExpression(obj: Expr, rest: List[~[String, Option[List[Expr]]]]) = {
    var t = obj
    def buildPath(xs: List[~[String, Option[List[Expr]]]]): Unit = xs match {
      case ~(meth, Some(args)) :: rest =>
        t = Apply(t, meth, args)
        buildPath(rest)
      case ~(field, None) :: rest =>
        t = Select(t, field)
        buildPath(rest)
      case Nil => 
    }
    buildPath(rest)
    t
  }
  
  import Type._

  def path(t: Expr, reduce: Expr => Expr): Stream[Expr] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case EvaluationException(_) =>
        Stream.cons(t, Stream.empty)
    }

  
  def eval(t: Tree): Tree = {
//    PrettyPrinter(t)
    t match {
      case Program(cls, expr) =>
        try { 
          cls foreach { cl => typeOf(cl, Nil) }
          val typeExpr = typeOf(expr,Nil)
          println("TYPE EXPR: "+typeExpr)
  
          println("EVALUATE TO: (One step evaluation)")
          for (t <- path(expr, Evaluate))
            println(t)
//	        val evExpr = Evaluate(expr)
//          print("EVALUATE TO: ")
//          evExpr
					
        } catch {
          case e @ TypeError(msg) => 
            println("Type Error: "+msg)
            e.printStackTrace()
            print("The expression will not be evaluated. Expr: ")
          case EvaluationException(msg) => 
            println("The expression generate an exception in Java: "+msg)
          case e @ _ => 
            println(e)
        }
        CT.clear
        expr
      case _ => 
        println("The file must start with a class definition")
        exit(-1)
    }
  }
  
  import java.io._

  def main(args: Array[String]): Unit = {
    val inputStream = if (args.length > 0) new FileInputStream(args(0)) else System.in
    val tokens = new lexical.Scanner(StreamReader(new InputStreamReader(inputStream)))
    //val tokens = new lexical.Scanner(args(0))
    phrase(Prog)(tokens) match {
      case Success(trees, _) =>
        try {
          //println(eval(trees))
					eval(trees)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }

}
