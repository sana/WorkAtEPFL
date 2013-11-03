package fos

import scala.collection.immutable.{Set, ListSet, HashMap}

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => "(" + a + " -> " + b + ")"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}

case class TypeVar(x: String) extends Type
case class TypeFun(t1: Type, t2: Type) extends Type
case object TypeNat extends Type
case object TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  override def toString() = args.mkString("[", ", ", "].") + tp

  private var counter:Int = 0
  def newVariable(name:String):Type = { counter += 1 ; TypeVar(name + counter) }

  /* def instantiate:Type = tp */
  def instantiate:Type = {
    // new Subst(new HashMap()) is an empty substitution
    (new Subst(new HashMap()) /: args) {
      (s, tv) =>
        tv match {
          case TypeVar(x) => s.append((x, newVariable(x)))
          case _ => s
        }
    } (tp)
  }
}

object Type {
  def FV(t: Type): List[String] = t match {
    case TypeVar(a) => a :: Nil
    case TypeFun(a, b) => FV(a) ::: FV(b)
    case TypeNat => Nil
    case TypeBool => Nil
  }
}

abstract class Substitution extends (Type => Type) {
  def apply(tp: Type): Type = tp match {
    case TypeVar(a) =>  lookup(TypeVar(a))
    case TypeFun(a, b) => TypeFun(this.apply(a), this.apply(b))
    case x => x
  }
  
  override def toString() = ""
  
  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] = {
    env map (x => x match { case (a, b) => (a, TypeScheme(b.args, this(b.tp))) } )
  }

  def append(s: Substitution): Substitution
  def append(p: (String, Type)): Substitution
  def domain(): List[String]
  def lookup(t: TypeVar): Type
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def append(s: Substitution) = s
  def append(p: (String, Type)) = new Subst(new HashMap() + p)
  def domain(): List[String] = Nil
  def lookup(t: TypeVar): Type = t
}

class Subst(m: Map[String, Type]) extends Substitution
{
  def append(s: Substitution) = {
    var m: Map[String, Type] = new HashMap()
    for (it <- s.domain)
      m = m + Pair(it, this(s(TypeVar(it))))
    for (it <- this.domain)
    {
      if (!(s.domain.contains(it)))
        m = m + Pair(it, this(TypeVar(it)))
    }
    new Subst(m)
  }
  
  def append(p: (String, Type)) = append(emptySubst.append(p))

  def domain(): List[String] = m.keys.toList
  def lookup(t: TypeVar): Type = {
    /** Look for the variable in map */
    m.contains(t.x) match {
      case false => t
      case true => m(t.x)
    }
  }
}

