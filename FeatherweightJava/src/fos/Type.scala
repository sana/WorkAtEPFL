package fos

import scala.collection.mutable.{Map,HashMap}

case class TypeError(msg: String) extends Exception(msg)

object Type {
  import CT._
  import Utils._
  
  type Class = String
  type Context = List[(Class, String)]
  
  def typeOf(tree: Tree, ctx: Context): Class = {
    tree match {

			//T-CLASS
      case ClassDef(cls, superclass, fields, ctor, methods) => {
        val superclsDef = getClassDef(superclass)
        val superclsFields = superclsDef.getFields // fields(D) = Di gi;
        val superclsNameFields = superclsFields map (_.name) 
  
				// constructor verify: 
        ctor match {

          case CtrDef(nameCtr, argsCtr, supersCtr, bodyCtr) => {
                
            val thisSupersNames = supersCtr map (_.name)

              // verify: super(gi) has the same arguments as fields(D)=Di gi
            if (superclsNameFields.length == thisSupersNames.length) {
               for (i <- 0 until superclsNameFields.length){
                    if(superclsNameFields(i) != thisSupersNames(i))
                        throw new TypeError("[Class def " + cls + " - constructor] parameter names from super() -" + thisSupersNames + "- differ from superclass fields name -" + superclsNameFields)
               }
            }  
            else
              throw new TypeError("[Class def " + cls + " - constructor] superclass fields and this super() have different numbers :" + supersCtr.mkString("(",",",")") + " to " + superclsNameFields.mkString("(",",",")"))

            //verify the two conditions for this rule to apply
            if (supersCtr.length + bodyCtr.length == argsCtr.length) {

							//first parameters of the constructor are the same as superclass fields
               for (i <- 0 until superclsFields.length){
                    if(superclsFields(i) != argsCtr(i))
                        throw new TypeError("[Class def " + cls + "- constructor args] constructor parameters -" + argsCtr + "- do not include superclass fields-" + superclsFields)
               }

								//last parameters of the constructor are the same as the fields from assigns this.fi=fi
               for (i <- superclsFields.length until argsCtr.length){
									val j = i - superclsFields.length
               		if(bodyCtr(j).obj != "this" || bodyCtr(j).field != bodyCtr(j).rhs.name )
							 			throw new TypeError("[Class def " + cls + "- constructor body] body assign -"+ bodyCtr(j) + "- is not of type this.fi=fi")
									if(bodyCtr(j).field != argsCtr(i).name )
								 		throw new TypeError("[Class def " + cls + "- constructor body] body assign -"+ bodyCtr(j) + "- does not correspond to any parameter of the constructor from this class ")
               }
            }  
            else
              throw new TypeError("[Class def " + cls + "- constructor args] superclass fields -" + superclsFields.mkString("(",",",")")+ "-  plus assigns -" + bodyCtr + "- are not as many as constructor parameters -" + argsCtr.mkString("(",",",")") )
						if(bodyCtr.length == fields.length){     
							for ( i <- 0 until bodyCtr.length){
								val j = i + superclsFields.length
								if(fields(i) != argsCtr(j))
									throw new TypeError("[Class def " + cls + "] field of class " + argsCtr(j) + " is not the same as its corresponding parameter of the constructor: "+ fields(i))
							}
						}
						else
							throw new TypeError("[Class def " + cls+ "] fields of the class are not as many as the assigns from the constructor")
			    }
          case _ => throw new TypeError("[Class def " + cls + "] constructor " + ctor + " not found")
      }           

			for(i <- 0 until methods.length)
				if(typeOfMethod(methods(i), cls) != "OK IN " + cls)
					throw new TypeError("[Class def " + cls + "] method " + methods(i) + "is not OK in class " + cls)

      "OK"
    }
    case _ => typeOfExpr(tree, ctx)
  }
}

	//T_METHOD:
	def typeOfMethod(method : MethodDef, cls : Class) = {
		method match { 
			case	MethodDef(tpe, name, args, body) => {
				/*
       	C0 m (C_bar x_bar) {return e0;}  OK IN C 			
				*/

				//CT(C) = class C extends D {....}
	      val clsDef = getClassDef(cls)
				val supercls = clsDef.superclass
				val superclsDef = getClassDef(supercls)

				// override(m,D, C_bar->C0) : mtype(m,D) = D_bar->D0 implies C_bar = D_bar && C0 = D0
        val superclsMethod = superclsDef.findMethod(method.name) match {
          case Some(s) => s
          case _ => null //Method is not defined in superclass
        }
				if(superclsMethod != null){			
					if(superclsMethod.tpe != method.tpe)
						throw new TypeError("[Method "+ method + "] override not consistent")
					if(superclsMethod.args.length != method.args.length)
						throw new TypeError("[Method "+ method + "] override not consistent")
					for(i <- 0 until method.args.length)
						if(method.args(i).tpe != superclsMethod.args(i).tpe)
							throw new TypeError("[Method "+ method + "] override not consistent")
				}

				//x_bar:C_bar, this:C |- e0 : E0  && E0 <: C0
	      val tpeDef = getClassDef(method.tpe)
				var context = (cls,"this" ) :: Nil;  
				for(i<- 0 until method.args.length)
					context = (method.args(i).tpe, method.args(i).name) :: context;
				val body_tpe = typeOfExpr(method.body, context)

				if(!tpeDef.isSuperClassOf(body_tpe))
					throw new TypeError("[Method " + method + "] type of body is not subtype of the return type ")

			}
			case _ => throw new TypeError("[Method " + method +  "] invalid method")
		}
		"OK IN " + cls
	}

  def typeOfExpr(tree: Tree, ctx: Context): Class = {
    tree match {
      // T-VAR
      case Var(x) =>
        ctx find { e => e._2 == x } match {
            case Some((tpe, x)) => tpe
            case None => throw new TypeError("[Variable] variable " + x + " is not bounded by current context " + ctx)
        }

      // T-New
      case New(cls, args) => {
        val clsDef = getClassDef(cls)
        val typeFields = clsDef.getFields map (_.tpe)
        val argsType = args map (typeOfExpr(_, ctx))

        if (typeFields.length == argsType.length) {
          for (i <- 0 until typeFields.length)
          {
            val tfDef = getClassDef(typeFields(i))
            if (!tfDef.isSuperClassOf(argsType(i)))
              throw new TypeError("[New] " + argsType(i) + " is not subtype of " + typeFields(i))
          }
        }
        else
          throw new TypeError("[New] can't type constructor " + argsType.mkString("(",",",")") + " to " + typeFields.mkString("(",",",")"))
        cls
      }

      case Cast(cls, e) => {
        val d = typeOfExpr(e, ctx)
        val dDef = getClassDef(d)
        // T-UCast
        if (dDef.isSubClassOf(cls))
          return cls
        // T-DCast
        if (dDef.isSuperClassOf(cls) && d != cls)
          return cls
        // T-SCast
        return cls
      }

      case Select(obj, field) => {
        // T-Field
        val c0 = typeOfExpr(obj, ctx)
        val c0Def = getClassDef(c0)
        val f = c0Def.findField(field)
        f match {
          case Some(FieldDef(tpe, name)) => tpe
          case _ => throw TypeError("[Select] field " + field + " not declared")
        }
      }
    
      // T-Invk
      case Apply(obj, method, args) => {
        val c0 = typeOfExpr(obj, ctx)
        val c0Def = getClassDef(c0)
        val c0Method = c0Def.findMethod(method) match {
          case Some(s) => s
          case _ => throw TypeError("[Apply] method " + method + " does not exist in class " + c0)
        }

        val c0Args = c0Method.args
        val typec0Args = c0Args map (_.tpe)
        val argsType = args map (typeOfExpr(_, ctx))

        if (typec0Args.length == argsType.length) {
          for (i <- 0 until typec0Args.length)
          {
            val tfDef = getClassDef(typec0Args(i))
            if (!tfDef.isSuperClassOf(argsType(i)))
              throw new TypeError("[Apply] " + argsType(i) + " is not subtype of " + typec0Args(i))
          }
        }
        else
          throw new TypeError("[Apply] can't invoke method " + c0Method + " with parameters " + argsType.mkString("(",",",")"))

        c0Method.tpe
      }

      case _ => null
    }
  }
}

case class EvaluationException(msg: String) extends Exception

object Evaluate extends (Expr => Expr) {
  import Utils._
  
  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef,Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
    case New(cls, args) => New(cls, args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(cls, e) => Cast(cls, substituteInBody(e, thiss, substs))
    case Var("this") => thiss
    case Var(bd) => substs find (_._1.name == bd) match {
      case None => exp
      case Some((_,sub)) => sub
    }
    case Apply(obj,method,args) => Apply(substituteInBody(obj, thiss, substs), method, args map { e => substituteInBody(e, thiss, substs) } )
    case _ => throw EvaluationException("Apply: Forgot expression "+exp)
  }
  
	// the only values are : new C(v1, ... vn) 
	def isValue(expr: Expr) : Boolean = {
		expr match {
			case New(cls, args) => {
				for (i <- 0 until args.length){
					if(isValue(args(i)) == false)
						 return false	
				}
				return true
			}
				case _ => false
		}
	}


  def apply(expr: Expr): Expr = {
		expr match {
			case New(cls, args) => {
				if(isValue(expr))
					throw new EvaluationException("" + expr)
				//RC-NEW-ARG
				var new_args:List[Expr] = Nil
				for (i <- 0 until args.length){
					if(isValue(args(i)) == false){
						new_args = new_args ::: List(apply(args(i)))
					}
					else
						new_args = new_args ::: List(args(i))
				}
				New(cls, new_args)
			}
			case Cast(cls, e) => {
				//R-Cast
				if(isValue(e) == true){
					e match {
						case New(c, args) =>{ 
							if(getClassDef(c).isSubClassOf(cls) == true)
								return e
						}
						case _ => throw new EvaluationException("[Evaluation] select error " + e)
					}
				}
				
				//RC-Cast
				return Cast(cls, apply(e))
			}
			case Select(obj, field) => {
					if(isValue(obj) ==true){
						obj match {
							//R-Field
							case New(cls, args) => {
	        			val clsDef = getClassDef(cls)
        				val nameFields = clsDef.getFields map (_.name)
								for (i <- 0 until nameFields.length)
									if(nameFields(i) == field)
										return args(i)
								return null
							}
							case _ => throw new EvaluationException("[Evaluation] value error " + expr)
 						}
					}
					else
						//RC-Field
						return Select(apply(obj) ,field)
			}

			case Apply(obj, method, args) => {
					if(isValue(obj) ==true){
						obj match {
							//R-INVK
							case New(cls, args_obj) => {
	        			val clsDef = getClassDef(cls)
								val m = clsDef.findMethod(method) match {
									case Some(s) => s
									case _ =>  throw new EvaluationException("[Evaluation] apply error " + expr)
								}								
								m match{
									case MethodDef(tpe_m, name_m, args_m, body_m) => {
										var substs:List[(FieldDef,Expr)] = Nil
										for( i <- 0 until args.length)
											substs =  (args_m(i) ,args(i) ) :: substs
										return substituteInBody(body_m, New(cls,args_obj), substs)
									}
									case _ => throw new EvaluationException("[Evaluation] apply error")
								}
							}

							case _ => throw new EvaluationException("[Evaluation] apply error " + expr)
 						}
					}
					else
						//RC-INVK-RECV
						return Apply(apply(obj) ,method, args)
			}
			case _ => throw EvaluationException("[Evaluator] Stuck term " + expr)

		}
	}
}

object CT {
  val objectClass = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil) , Nil)
  private val ct = new HashMap[String, ClassDef]
   
  add(objectClass,objectClassDef)
  
  def elements = ct.iterator
  def lookup(classname: String): Option[ClassDef] = if(classname != null) ct get classname else None
  def add(key: String, element: ClassDef): Unit = ct += key -> element
  def delete(key: String) = ct -= key
  def clear(): Unit = {
    ct.clear
    add(objectClass,objectClassDef)
  }
}


object Utils {
  def getClassDef(className: String): ClassDef = CT lookup className match {
    case None => throw TypeError("class "+className+" not declared")
    case Some(c: ClassDef) => c
  }
}
