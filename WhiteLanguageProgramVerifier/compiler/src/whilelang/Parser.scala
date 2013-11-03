package whilelang

import java.io.{InputStream,IOException}

/** LL_TOKEN(1) parser for the while language grammar. */
class Parser extends Lexer {  
  def parseInputStream(in: InputStream): Command = {
    setInputStream(in)
    readToken 
    parseGoal
  }
  
  /** Store the current token, and one lookahead token, as read from the lexer. */
  private var currentToken: Token = _
  
  def readToken: Unit = {
    currentToken = nextToken
  }
  
  val anyId = ID_TOKEN("Identifier")
  val anyNumLit = new NUMLIT_TOKEN(0) { override def toString = "number literal" }
  val anyStrLit = STRLIT_TOKEN("String literal")
  
  /** ''Eats'' the expected token, or terminates with an error. */
  private def eat(token: Token): Unit = (currentToken, token) match {
    case (ID_TOKEN(_), ID_TOKEN(_)) => readToken
    case (NUMLIT_TOKEN(_), NUMLIT_TOKEN(_)) => readToken
    case (STRLIT_TOKEN(_), STRLIT_TOKEN(_)) => readToken
    case (a, b) if a == b => readToken
    case _ => expected(token)
  }
    
  /** Complains that what was found was not expected. The method accepts
   * arbitrarily many arguments of type TokenClass */
  private def expected(token: Token, more: Token*): Nothing = {
    Error("Expected: " + (token::more.toList).mkString(" or ") + ", found: " + currentToken)
  }
    
  private def parseGoal: Command = {
    val coms = parseCommands
    eat(EOF_TOKEN)
    Block(coms)
  }
  
  private def parseCommand: Command = currentToken match {
    case ID_TOKEN(_) => {
      val assignId = parseIdentifier
      eat(EQSIGN_TOKEN)

      currentToken match {

        case _ => {
          val expr = parseTerm
          eat(SEMICOLON_TOKEN)
          Assignment(assignId, expr)
        }
      }
    }
    case IF_TOKEN => {
      readToken
      eat(LPAREN_TOKEN)
      val cond = parseBoolExpr
      eat(RPAREN_TOKEN)
      val then = parseCommand
      currentToken match {
        case ELSE_TOKEN => {
          readToken
          val elze = parseCommand
          IfThenElse(cond, then, elze)
        }
        case _ => IfThenElse(cond, then, Skip())
      }
    }
    case WHILE_TOKEN => {
      readToken
      eat(LPAREN_TOKEN)
      val cond = parseBoolExpr
      eat(RPAREN_TOKEN)
      val com = parseCommand
      WhileLoop(cond, com)
    }

    case ASSUME_TOKEN => {
      readToken
      eat(LPAREN_TOKEN)
      val cond = parseBoolExpr
      eat(RPAREN_TOKEN)
      eat(SEMICOLON_TOKEN)
      Assume(cond)
    }

    case ASSERT_TOKEN => {
      readToken
      eat(LPAREN_TOKEN)
      val cond = parseBoolExpr
      eat(RPAREN_TOKEN)
      eat(SEMICOLON_TOKEN)
      Assert(cond)
    }

    case LBRACE_TOKEN => {
      readToken
      val ret = Block(parseCommands)
      eat(RBRACE_TOKEN)
      ret
    }

    case LPAREN_TOKEN => {
      readToken
      val c1 = parseCommand
      eat(BOX_TOKEN)
      val c2 = parseCommand
      eat(RPAREN_TOKEN)
      Choice(c1, c2)
    }

    case LOOP_TOKEN => {
      readToken
      val com = parseCommand
      Loop(com)
    }

    case HAVOC_TOKEN => {
      readToken
      eat(LPAREN_TOKEN)
      val v = parseId
      eat(RPAREN_TOKEN)
      eat(SEMICOLON_TOKEN)
      Havoc(v)
    }

    case _ => expected(anyId, IF_TOKEN, WHILE_TOKEN, LBRACE_TOKEN, ASSUME_TOKEN, ASSERT_TOKEN, LPAREN_TOKEN, LOOP_TOKEN, HAVOC_TOKEN)
  }
  
  private def parseCommands: List[Command] = {
    var lst: List[Command] = Nil
    while(isFirstOfCommand) {
      lst = parseCommand :: lst
    }
    lst.reverse
  }
  
  private def isFirstOfCommand: Boolean = currentToken match {
    case ID_TOKEN(_) | IF_TOKEN | WHILE_TOKEN | LBRACE_TOKEN | ASSUME_TOKEN | ASSERT_TOKEN | LPAREN_TOKEN | LOOP_TOKEN | HAVOC_TOKEN => true
    case _ => false
  }
  
  private def parseStringLit: String = currentToken match {
    case STRLIT_TOKEN(value) => { readToken; value }
    case _ => expected(anyStrLit)
  }
  
  private def parseIdentifier: String = currentToken match {
    case ID_TOKEN(value) => { readToken; value }
    case _ => expected(anyId)
  }
  
  // all expressions
  private def parseBoolExpr: BoolExpr = parseExpr7
  
  // expr7 ::= expr6 ('||' expr6)*
  private def parseExpr7: BoolExpr = {
    var e6 = parseExpr6
    while(currentToken == OR_TOKEN) {
      readToken
      e6 = Or(e6, parseExpr6)
    }
    e6
  }
  
  // expr6 ::= expr5 ('&&' expr5)*
  private def parseExpr6: BoolExpr = {
    var e5 = parseExpr5
    while(currentToken == AND_TOKEN) {
      readToken
      e5 = And(e5, parseExpr5)
    }
    e5
  }
  
  // expr5 ::= expr4 (('>' | '<' | '==') expr4)*
  private def parseExpr5: BoolExpr = {
    if(currentToken == BANG_TOKEN) {
      readToken
      Negation(parseExpr5)
    } else if(currentToken == LPAREN_TOKEN) {
      readToken
      val ret = parseBoolExpr
      eat(RPAREN_TOKEN)
      ret
    } else {
      var e4 = parseTerm
      if(currentToken == EQUALS_TOKEN) {
        readToken
        Equal(e4, parseTerm)
      } else if(currentToken == NOTEQUALS_TOKEN) {
        readToken
        Negation(Equal(e4, parseTerm))
      } else if(currentToken == LT_TOKEN) {
        readToken
        LessThan(e4, parseTerm)
      } else if(currentToken == GT_TOKEN) {
        readToken
        GreaterThan(e4, parseTerm)
      } else if(currentToken == LEQ_TOKEN) {
        readToken
        Negation(LessThan(parseTerm, e4))
      } else if(currentToken == GEQ_TOKEN) {
        readToken
        Negation(GreaterThan(parseTerm, e4))
      } else {
        expected(EQUALS_TOKEN, LT_TOKEN, GT_TOKEN, LEQ_TOKEN, GEQ_TOKEN)
      }
    }
  }
  
  private def parseTerm = parseExpr4

  // expr4 ::= expr3 (('+' | '-') expr3)*
  private def parseExpr4: Term = {
    var e3 = parseExpr3
    while(currentToken == PLUS_TOKEN || currentToken == MINUS_TOKEN) {
      if(currentToken == PLUS_TOKEN) {
        readToken
        e3 = Plus(e3, parseExpr3)
      } else {
        readToken
        e3 = Minus(e3, parseExpr3)
      }
    }
    e3
  }
  
  // expr3 ::= expr2a (('*' | '/' | '%') expr2a)*
  private def parseExpr3: Term = {
    var e2 = parseExpr2a
    while(currentToken == DIV_TOKEN || currentToken == TIMES_TOKEN) {
      if(currentToken == DIV_TOKEN) {
        readToken
        val e2b = parseExpr2a
        e2 = e2b match {
          case Constant(c) => Divide(e2, c)
          case _ => Error("Division by a non-constant term.")
        }
      } else if(currentToken == TIMES_TOKEN) {
        readToken
        e2 = e2 match {
          case Constant(c) => Times(c, parseExpr2a)
          case _ => Error("Multiplication by a non-constant term.")
        }
      } 
    }
    e2
  }
  
  // expr2a ::= !expr2a | expr2b
  private def parseExpr2a: Term = parseExpr2b
  private def parseExpr2b = parseExpr1
  
  private def parseExpr1: Term = currentToken match {
      case NUMLIT_TOKEN(value) => { val ret = Constant(value); readToken; ret }
      case LPAREN_TOKEN => { readToken; val res = parseTerm; eat(RPAREN_TOKEN); res }
      case ID_TOKEN(value) => { readToken; Variable(value) }      
      case _ => expected(anyNumLit, LPAREN_TOKEN, anyId)
  }

  private def parseInt: Int = currentToken match {
    case NUMLIT_TOKEN(value) => { readToken; value }
    case _ => expected(anyNumLit)
  }

  private def parseId: Variable = currentToken match {
    case ID_TOKEN(value) => { readToken; Variable(value) }
    case _ => expected(anyId)
  }
}
