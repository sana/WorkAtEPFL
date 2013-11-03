package fos

abstract class TypeInferencers {
  import Type._

  type Env = List[(String, TypeScheme)]

  case class TypeError(msg: String) extends Exception(msg)

  /** Lookup variable <code>name</code> in the given environment. */
  def lookup(env: Env, name: String): TypeScheme = env match {
    case Nil => null
    case (n, tp) :: env1 => if (n == name) tp else lookup(env1, name)
  }

  /** Turn a syntactic type (given explicitly) into a proper type. */
  def toType(s: TypeTree): Type = s match {
    case BoolType => TypeBool
    case NatType  => TypeNat
    case FunType(t1, t2) => TypeFun(toType(t1), toType(t2))
  }

  def typeOf(t: Term): Type;
}
