package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noContraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /** Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    // CT-Var
    case Var(x) =>
      val t1 = lookup(env, x)
      if (t1 == null)
        throw TypeError("Unknown variable " + x)
      TypingResult(t1.instantiate, noContraints)
    // CT-Abs
    case Abs(x, tp, t) =>
      val tpe = tp match {
        case EmptyType => newVariableType
        case _ => toType(tp)
      }
      val result = collect((x, new TypeScheme(Nil, tpe)) :: env, t)
      TypingResult(TypeFun(tpe, result.tpe), result.c)
    // CT-App
    case App(t1, t2) =>
      val result_t1 = collect(env, t1)
      val result_t2 = collect(env, t2)
      val tpe = newVariableType
      val constraints = new Constraint(result_t1.tpe, TypeFun(result_t2.tpe, tpe)) :: (result_t1.c ::: result_t2.c)
      TypingResult(tpe, constraints)
    // CT-True
    case True => TypingResult(TypeBool, noContraints)
    // CT-False
    case False => TypingResult(TypeBool, noContraints)
    // CT-Zero
    case Zero => TypingResult(TypeNat, noContraints)
    // CT-IsZero
    case IsZero(t) => val result = collect(env, t)
        TypingResult(TypeBool, new Constraint(result.tpe, TypeNat) :: result.c)
    // CT-Succ
    case Succ(t) => val result = collect(env, t)
        TypingResult(TypeNat, new Constraint(result.tpe, TypeNat) :: result.c)
    // CT-Pred
    case Pred(t) => val result = collect(env, t)
        TypingResult(TypeNat, new Constraint(result.tpe, TypeNat) :: result.c)
    // CT-If
    case If(cond, t1, t2) =>
        var result_cond = collect(env, cond)
        var result_t1 = collect(env, t1)
        var result_t2 = collect(env, t2)
        val constraints = new Constraint(result_t1.tpe, result_t2.tpe) :: ((new Constraint(result_cond.tpe, TypeBool) ::
                (result_cond.c ::: result_t1.c)) ::: result_t2.c);
        TypingResult(result_t1.tpe, constraints)
    // CT-Let
    case Let(x, v, t) =>
      // type the right hand side v obtaining a type S and a set of constraints C
      var result_v = collect(env, v)
      // use unification on C and apply the result to S to find its first
      // approximation as type; at this point, the substitution we found
      // should be applied to the current environment too, since we have
      // committed to a set of bindings between type variables and types
      val substitution = unify(result_v.c)
      val newType = substitution(result_v.tpe)
      var newEnviroment = substitution.apply(env)

      // generalize some type variables inside T and obtain a type scheme
      val vars: List[TypeVar] = FV(newType).filter(x => !newEnviroment.contains(x)).map(name => TypeVar(name))
      val newTypeScheme = new TypeScheme(vars, newType)

      // extend the environment with a binding from x to its type scheme
      newEnviroment = (x, newTypeScheme) :: newEnviroment

      // each time x appears in t, its type scheme will be instantiated
      // and used as a type for x
      collect(newEnviroment, t)
  }

  /**
   * if C.isEmpty then []
   * else let {S = T} U C' = C in
   *   (1) if S = T
   *     then unify(C')
   *   (2) else if S = X and X not in FV(T)
   *     then unify([X -- T] C') o [X -- T]
   *   (3) else if T = X and X not in FV(T)
   *     then unify([X -- S] C') o [X -- S]
   *   (4) else if S = S1 -- S2 and T = T1 -- T2
   *     then unify(C' U [S1 = T1, S2 = T2])
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeBool, TypeBool) => unify(c.tail)
      case (TypeNat, TypeNat) => unify(c.tail)
      // (1)
      case (TypeVar(a), TypeVar(b)) if (a == b) => unify(c.tail)
      // (2)
      case (TypeVar(a), t) if (!FV(t).contains(a)) =>
        val substitution = emptySubst.append((a, t))
        unify(c.tail.map(p => substitution(p))).append(substitution)
      // (3)
      case (s, TypeVar(a)) if (!FV(s).contains(a)) =>
        val substitution = emptySubst.append((a, s))
        unify(c.tail.map(p => substitution(p))).append(substitution)
      // (4)
      case (TypeFun(s1, s2), TypeFun(t1, t2)) => unify((s1, t1) :: (s2, t2) :: c.tail)
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

  /** Creates a new fresh type */
  var counter:Int  = 0
  def newVariableType(): TypeVar = {
    counter = counter + 1
    TypeVar("Type" + counter)
  }
}

