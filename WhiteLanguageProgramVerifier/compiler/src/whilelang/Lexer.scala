package whilelang

import java.io.{InputStream, IOException}

sealed abstract class Token
case object EOF_TOKEN extends Token           { override def toString = "<EOF>" }
case object WHILE_TOKEN extends Token         { override def toString = "while" }
case object IF_TOKEN extends Token            { override def toString = "if" }
case object ELSE_TOKEN extends Token          { override def toString = "else" }
case object ASSUME_TOKEN extends Token        { override def toString = "assume" }
case object ASSERT_TOKEN extends Token        { override def toString = "assert" }
case object SEMICOLON_TOKEN extends Token     { override def toString = ";" }
case object LPAREN_TOKEN extends Token        { override def toString = "(" }
case object RPAREN_TOKEN extends Token        { override def toString = ")" }
case object EQSIGN_TOKEN extends Token        { override def toString = "=" }
case object LBRACE_TOKEN extends Token        { override def toString = "{" }
case object RBRACE_TOKEN extends Token        { override def toString = "}" }
case object BANG_TOKEN extends Token          { override def toString = "!" }
case object PLUS_TOKEN extends Token          { override def toString = "+" }
case object MINUS_TOKEN extends Token         { override def toString = "-" }
case object TIMES_TOKEN extends Token         { override def toString = "*" }
case object DIV_TOKEN extends Token           { override def toString = "/" }
case object LT_TOKEN extends Token            { override def toString = "<" }
case object GT_TOKEN extends Token            { override def toString = ">" }
case object GEQ_TOKEN extends Token           { override def toString = ">=" }
case object LEQ_TOKEN extends Token           { override def toString = "<=" }
case object EQUALS_TOKEN extends Token        { override def toString = "==" }
case object NOTEQUALS_TOKEN extends Token     { override def toString = "!=" }
case object AND_TOKEN extends Token           { override def toString = "&&" }
case object OR_TOKEN extends Token            { override def toString = "||" }
case object BOX_TOKEN extends Token           { override def toString = "[]" }
case object LOOP_TOKEN extends Token           { override def toString = "loop" }
case object HAVOC_TOKEN extends Token           { override def toString = "havoc" }
case class NUMLIT_TOKEN(value: Int) extends Token { override def toString = value.toString }
case class ID_TOKEN(value: String) extends Token { override def toString = value }
case class STRLIT_TOKEN(value: String) extends Token { override def toString = "\""+value+"\"" }

class Lexer {  
  val keywords = Map[String,Token](
    ("if" -> IF_TOKEN),
    ("else" -> ELSE_TOKEN),
    ("while" -> WHILE_TOKEN),
    ("assume" -> ASSUME_TOKEN),
    ("loop" -> LOOP_TOKEN),
    ("havoc" -> HAVOC_TOKEN),
    ("assert" -> ASSERT_TOKEN))
  
  // the input stream
  private var in: InputStream = _
  // the last char seen in the input stream
  private var currentChar: Char = _
  // the current token type under consideration (useless if not an identifier or a literal)
  private var currentToken: Token = _
  // a buffer to store textual information for parametric tokens
  private val buffer: StringBuffer = new StringBuffer
  
  /** Sets the input stream to use for lexing. */
  def setInputStream(input: InputStream): Unit = { in = input; nextChar }
  
  /** What will mark the end of the input stream. */
  private val EndOfFile: Char = java.lang.Character.MAX_VALUE
  
  /** Used to detect \r\n pairs and ouput only \n for them. */
  private var previousChar: Char = _
  
  /** Puts the next character in the input stream in currentChar, or the special character EndOfFile if the stream is exhausted. */
  private def nextChar: Unit = {
    def readChar: Char = in.read match {
      case c if c < 0 => EndOfFile
      case c => c.asInstanceOf[Char]
    }

    currentChar match {                                                                                                                                                      
      case EndOfFile => return
      case _ => ;                                                                                                                 
    }
    
    try {                                                                                                                                                           
      currentChar  = readChar
      previousChar = if ((previousChar == '\r') && (currentChar == '\n')) readChar else currentChar                                                                                                   
      currentChar = if (previousChar == '\r') '\n' else previousChar                                                                                                                       
    } catch {                                                                                                                                                         
      case e: IOException => Error(e.getMessage)                                                                                              
    }
  }
  
  /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
  def nextToken: Token = {
    while(currentChar == '/' || Character.isWhitespace(currentChar)) {
      if(currentChar == '/') {
        nextChar
        
        if(currentChar == '/') {
          // skips end-of-line comments
          while(currentChar != '\n' && currentChar != EndOfFile) nextChar
        } else if(currentChar == '*') {
          // skips block comments
          var foundEnd: Boolean = false
          while(!foundEnd) {
            while(currentChar != '*') {
              if(currentChar == EndOfFile) Error("Unterminated block comment")
              nextChar
            }
            nextChar
            if(currentChar == '/') { foundEnd = true; nextChar }
          }
        } else {
          return DIV_TOKEN
        }
      } else {
        nextChar
      }
    }
    
    return readToken
  }
  
  /** Reads the next token from the stream. */
  private def readToken: Token = {    
    currentChar match {
      case EndOfFile => EOF_TOKEN
    
      case _ if Character.isLetter(currentChar) => {
        buffer.setLength(0)
        do {
          buffer.append(currentChar)
          nextChar
        } while (Character.isLetterOrDigit(currentChar) || currentChar == '_');
        val str: String = buffer.toString
        keywords.get(str) match {
          case None => ID_TOKEN(str)
          case Some(token) => token
        }
      }
    
      case '0' => { nextChar; NUMLIT_TOKEN(0) }
      
      case _ if Character.isDigit(currentChar) => {
        buffer.setLength(0)
        do {
          buffer.append(currentChar)
          nextChar
        } while (Character.isDigit(currentChar))
        val num: Int = Integer.parseInt(buffer.toString)
        NUMLIT_TOKEN(num)
      }
    
      case '"' => {
        buffer.setLength(0)
        nextChar
        while (currentChar != '"') {
          if (currentChar == '\n' || currentChar == EndOfFile) {
            Error("Unterminated string")
          }
          buffer.append(currentChar)
          nextChar
        }
        nextChar
        val str: String = buffer.toString
        STRLIT_TOKEN(str)
      }
    
      case ';' => { nextChar; SEMICOLON_TOKEN }
      case '!' => { nextChar; if(currentChar == '=') { nextChar; NOTEQUALS_TOKEN } else { BANG_TOKEN } }
      case '(' => { nextChar; LPAREN_TOKEN }
      case ')' => { nextChar; RPAREN_TOKEN }
      case '{' => { nextChar; LBRACE_TOKEN }
      case '}' => { nextChar; RBRACE_TOKEN }
      case '<' => { nextChar; if(currentChar == '=') { nextChar; LEQ_TOKEN } else { LT_TOKEN } }
      case '>' => { nextChar; if(currentChar == '=') { nextChar; GEQ_TOKEN } else { GT_TOKEN } }
      case '+' => { nextChar; PLUS_TOKEN }
      case '-' => { nextChar; MINUS_TOKEN }
      case '*' => { nextChar; TIMES_TOKEN }
      case '=' => { nextChar; if(currentChar == '=') { nextChar; EQUALS_TOKEN } else { EQSIGN_TOKEN } }
      case '&' => { nextChar; if(currentChar == '&') { nextChar; AND_TOKEN } else { Error("Single '&'") } }
      case '|' => { nextChar; if(currentChar == '|') { nextChar; OR_TOKEN } else { Error("Single '|'") } }
      case '[' => { nextChar; if(currentChar == ']') { nextChar; BOX_TOKEN } else { Error("'[' withouth following ']'") } }
    
      case _ => { Error("Invalid character: " + currentChar) }
    }
  }
}
