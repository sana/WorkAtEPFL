package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

import java.io.PrintWriter
import scala.collection.immutable.{Map,ListMap}

import CT._
import Utils._

abstract class TreeException(msg: String) extends Exception

abstract class ClassException(msg: String) extends TreeException(msg)
case class ClassConstructorArgsException(msg: String) extends ClassException(msg)

abstract class MethodException(msg: String) extends TreeException(msg)
case class MethodArgsException(arg: String) extends MethodException(arg)
case class MethodOverrideException(msg: String) extends MethodException(msg)
case class MethodArgsLengthException(msg: String) extends MethodException(msg)

abstract class FieldException(msg: String) extends TreeException(msg)
case class FieldAlreadyDefined(msg: String) extends FieldException(msg)

sealed abstract class Tree extends Positional

case class Program(cls: List[ClassDef], expr: Expr) extends Tree {
  cls foreach { c => CT.add(c.name,c) }
}

case class ClassDef(name: String, superclass: String, fields: List[FieldDef], ctor: CtrDef, methods: List[MethodDef]) extends Tree {
  private def fieldLookup: List[FieldDef] = CT lookup superclass match {
    case Some(s) => s.fieldLookup ::: fields 
    case _ => fields                              
  }
 
  def getFields: List[FieldDef] = fieldLookup
  def getFieldsSuperclass: List[FieldDef] = getClassDef(superclass).fieldLookup
  def findField(fieldName: String): Option[FieldDef] = fieldLookup.find(_.name == fieldName) 
  def checkFields: Unit = checkListFieldsDef(fieldLookup)
  
  /**
   * Verify that in the list there is no two occurrence of the same variable name
   */
  private def checkListFieldsDef(f: List[FieldDef]): Unit =
    f find { field => f.count(_.name == field.name) > 1 } match {
      case Some(varfield) => throw FieldAlreadyDefined("variable "+varfield.name+" is already defined in the scope")
      case _ => 
    }
  
  private def methodLookup: List[MethodDef] = CT lookup superclass match {
    case Some(m) => methods ::: m.methodLookup filter { m => methods exists (_.name == m.name) }
    case _ => methods
  }
  
  def findMethod(methodName: String): Option[MethodDef] = methods find (_.name == methodName) match {
    case Some(method) => Some(method)
    case _ => CT lookup superclass match {
      case Some(superc) => superc findMethod methodName
      case _ => None
    }
  }
  
  def overrideMethod(tpe: String, name: String, args: List[FieldDef], body: Expr): Unit = {
    if(methods.count(_.name == name) > 1) throw new MethodOverrideException(", method "+name+" is defined more than once")
    try {
      checkListFieldsDef(args)
    } catch {
      case FieldAlreadyDefined(msg) => throw FieldAlreadyDefined("In class "+this.name+", in the arguments of method "+name+", "+msg) 
    }
    val inheritedMethod = getClassDef(superclass).findMethod(name)
    inheritedMethod match {
      case Some(MethodDef(tpeS, name, argsS, _)) => 
        var error = false
        if(tpe == tpeS) {
          val paramsOvMethod = args map (_.tpe)
          val paramsInMethod = argsS map (_.tpe)
          if(args.length != argsS.length) 
            throw new MethodOverrideException("can't apply method "+paramsOvMethod.mkString("(",",",")")+" to "+paramsInMethod.mkString("(",",",")"))
          for (i <- 0 until paramsInMethod.length) {
            if((paramsInMethod(i) != paramsOvMethod(i)) && !error) error = true
          }  
          if(error)
            throw new MethodOverrideException("can't apply method "+paramsOvMethod.mkString("(",",",")")+" to "+paramsInMethod.mkString("(",",",")"))
          //Everything was ok, so override ok
        } else
          throw new MethodOverrideException("Type mismatch. The return type "+tpeS+" of inherithed "+
            "method "+name+" has different signature. Overriding method has type "+tpe)
      case _ => 
    }
  }
  
  def checkTypeArguments(argsType: List[String]): Unit = {
    var errorSub = (false, 0)
    val typeFields = fieldLookup map (_.tpe)
    if(typeFields.length == argsType.length) {
      for (i <- 0 until typeFields.length) {
        if(!getClassDef(argsType(i)).isSubClassOf(typeFields(i)) && !errorSub._1) errorSub = (true,i)
      }
      if(errorSub._1) 
        throw new ClassConstructorArgsException("can't apply constructor "+argsType.mkString("(",",",")")+" to "+typeFields.mkString("(",",",")")+" because "+
          argsType(errorSub._2)+" is not a subtype of "+typeFields(errorSub._2))
      //no errors means everything was fine
    } else 
      throw new ClassConstructorArgsException("can't apply constructor "+argsType.mkString("(",",",")")+" to "+typeFields.mkString("(",",",")"))
  }
  
  /**
   * verifyConstructorArgs: verify the name and the type of each parameter in the cosntructor respect to the fields declared in the class
   * checkTypeArguments: verify only the type of the parameters not the name)
   */
  def verifyConstructorArgs: Unit = {
    try {
      checkListFieldsDef(ctor.args)
    } catch {
      case FieldAlreadyDefined(msg) => throw FieldAlreadyDefined(", in the constructor, "+msg)
    }
    val fieldss = fieldLookup
    val fieldsType = fieldss map (_.tpe)
    val constructType = ctor.args map (_.tpe)
    if (fieldss.length != ctor.args.length) 
      throw new ClassConstructorArgsException("can't apply constructor "+constructType.mkString("(",",",")")+
        " to "+fieldsType.mkString ("(",",",")"))
    (fieldss zip ctor.args) foreach { case (a, b) => 
      if(a != b) throw new ClassConstructorArgsException("can't apply constructor "+(ctor.args).mkString("(",",",")")+
        " to "+fieldss.mkString ("(",",",")"))
    }
    //No error means every parameter of the constructor has the same type and name of the ones in the class   
  }
    
    
  def superClass: Option[ClassDef] = CT.lookup(this.superclass) 
    
  def isSuperclassOf(that: Option[ClassDef]): Boolean = that match {
    case Some(sub) => (name == sub.name) || (this isSuperclassOf sub.superClass)
    case _ => false
  }

  def isSuperClassOf(that: Option[ClassDef]): Boolean = that match {
    case Some(sub) => (name == sub.name) || (this isSuperclassOf sub.superClass)
    case _ => false
  }
  def isSuperClassOf(that: String): Boolean = this isSuperclassOf Some(getClassDef(that))

  def isSubClassOf(that: ClassDef): Boolean = that isSuperclassOf Some(this)
  def isSubClassOf(that: String): Boolean = isSubClassOf(getClassDef(that))
}

case class FieldDef(tpe: String, name: String) extends Tree {
  override def toString = tpe+" "+name
}

case class CtrDef(name: String, args: List[FieldDef], supers: List[Var], body: List[Assign]) extends Tree
case class Assign(obj: String, field: String, rhs: Var) extends Tree

case class MethodDef(tpe: String, name: String, args: List[FieldDef], body: Expr) extends Tree {
  def checkTypeArguments(argsType: List[String]): Unit = {
    var error = false
    val params = args map (_.tpe)
    if(params.length == argsType.length) {
      for (i <- 0 until params.length) {
        if(!((getClassDef(argsType(i))) isSubClassOf params(i)) && !error) error = true
      }
      if(error) 
        throw new MethodArgsException("can't apply method "+argsType.mkString ("(",",",")")+" to "+params.mkString ("(",",",")"))
    } else 
      throw new MethodArgsException("can't apply method "+argsType.mkString ("(",",",")")+" to "+params.mkString ("(",",",")"))
  }
}

abstract class Expr extends Tree

case class Var(name: String) extends Expr {
  override def toString = name
}
case class New(cls: String, args: List[Expr]) extends Expr {
  override def toString = "new "+cls+""+args.mkString("(",",",")")
}
case class Cast(cls: String, e: Expr) extends Expr {
  override def toString = "( ("+cls+")"+e+")"
}
case class Select(obj: Expr, field: String) extends Expr {
  override def toString = obj+"."+field
}
case class Apply(obj: Expr, method: String, args: List[Expr]) extends Expr {
  override def toString = obj+"."+method+""+args.mkString("(",",",")")
}

/**
 * Pretty printer using scala.text formatting library. It works
 * by first building a document abstracting over nesting, spacing and
 * new lines, and afterwards rendering the text given a Writer and a
 * desired width.
 */
object PrettyPrinter  {
  def apply(t: Tree) = {
    val writer = new PrintWriter(System.out)
    toDocument(t).format(80, writer)
    writer.println
    writer.flush()
  }
  
  import scala.text._
  import scala.text.Document._
  
  def toDocument(ts: List[Tree], sep: String, suffix: String): Document = ts match {
    case one :: two :: rest =>
      toDocument(one) :: sep :/: rest.foldLeft(toDocument(two)) { (d, e) => 
        if (sep != "") d :: sep :/: toDocument(e) 
        else d :/: toDocument(e) } :: text(suffix)
    case one :: Nil =>
      toDocument(one) :: text(suffix)
    case Nil =>
      empty
  }
      
  def toDocument(t: Tree): Document = t match {
    case Program(cls, expr) =>
      group(toDocument(cls, "", "")) :/: group(toDocument(expr)) 
    case ClassDef(name, superclass, fields, ctor, methods) =>
      group("class " :: name :/: "extends " :: superclass :: empty) :: 
         nest(2, " {" :/: group(toDocument(fields, ";", ";") :/: toDocument(ctor) :/: toDocument(methods, "", ""))) :/:
      "}" :/: empty
    case FieldDef(tpe, name) =>
      group(tpe :/: text(name)) 
    case CtrDef(name, args, supers, body) =>
      group(name :: "(" :: group(toDocument(args, ",", "")) :: ")" :: empty) :/:
        nest(2,  "{" :/:
            group("super(" :: group(toDocument(supers, ",", "")) :: ");" :/: empty) :/:
            group(toDocument(body, "", "")) 
            ) :/:
      "}" :/: empty
    case Assign(obj, field, rhs) =>
      group(obj :: "." :: field :/: "=" :/: toDocument(rhs) :: ";" :/: empty)
    case MethodDef(tpe, name, args, body) =>
      group(tpe :/: name :/: "(" :: group(toDocument(args, ",", "")) :: ")" :/: text("{")) :/:
        nest(2, "return " :: group(toDocument(body))) :/:
      "}" :/: empty
    case Var(name) =>
      text(name)
    case New(cls, args) =>
      "new " :: cls :: "(" :: group(toDocument(args, ",", "") :: text(")"))
    case Cast(cls, expr) =>
      group("(" :: cls :: ")" :/: toDocument(expr))
    case Select(obj, field) =>
      toDocument(obj) :: "." :: text(field)
    case Apply(obj, meth, args) =>
      toDocument(obj) :: "." :: meth :: nest(2, "(" :: group(toDocument(args, ",", "")) :: text(")"))
    case _ => 
      super.toString :/: empty
  }
}

