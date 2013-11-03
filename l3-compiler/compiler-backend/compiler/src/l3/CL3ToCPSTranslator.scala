package l3

import l3.{ SymbolicCL3TreeModule => S }
import l3.{ SymbolicCPSTreeModule => C }

object CL3ToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree, _ => C.Halt)
  }

  private def nonTail(tree: S.Tree, ctx: Symbol => C.Tree): C.Tree =
    tree match {
      case S.Let((name, value) :: rest, body) =>
        tempLetK("lk", List(name), nonTail(S.Let(rest, body), ctx)) { lk =>
          tail(value, lk)
          //nonTail(value, v => C.AppK(lk, List(v)))
        }

      case S.Let(List(), body) =>
        nonTail(body, ctx)

      case S.Ident(name) => ctx(name)
      
      case S.Lit(value) => {
    	  val lSym = Symbol.fresh("lit")
    	  C.LetL(lSym, value, ctx(lSym))
      }
    
      case S.LetRec(functions, body) => {
    	  C.LetF(functions.map (f => {
    	 	  val k = Symbol.fresh("letrec")
    		  //C.FunDef(f.name, k, f.args, nonTail(f.body, v => C.AppK(k, List(v))))
    		  C.FunDef(f.name, k, f.args, tail(f.body, k))
    	  }), nonTail(body, ctx))
      }

      case S.App(fun, args) => {
    	  val k = Symbol.fresh("appf")
    	  val r = Symbol.fresh("cont")
    	  nonTail_*(fun :: args, list => {
    	 	  C.LetK(k, List(r), ctx(r), C.AppF(list.head, k, list.tail))
    	  })
      }
      
      case S.If(condE, thenE, elseE) => {
    	  condE match {
    	 	  case S.Prim(prim, args) => {
    	 	 	  prim match {
    	 	 	 	  case prim: L3TestPrimitive => {
    	 	 	 		  val k = Symbol.fresh("k")
    	 	 	 		  val r = Symbol.fresh("r")
    	 	 	 		  val tk = Symbol.fresh("tk")
    	 	 	 		  val ek = Symbol.fresh("ek")
    	 	 	 		  C.LetK(k, List(r), ctx(r),
    	 	 	 		    C.LetK(tk, List(), tail(thenE, k) /*nonTail(thenE, v2 => C.AppK(k, List(v2)))*/,
    	 	 	 		  	  C.LetK(ek, List(), tail(elseE, k) /*nonTail(elseE, v3 => C.AppK(k, List(v3)))*/,
						        nonTail_*(args, list => {
						    	  C.If(prim, list, tk, ek)
						        }))))
    	 	 	 	  }
    	 	 	 	  case _ => throw new Exception("L3TestPrimitive expected")
    	 	 	  }
    	 	  }
    	 	  case _ => {
    	 	 	  val k = Symbol.fresh("k")
    	 	 	  val r = Symbol.fresh("r")
    	 	 	  val tk = Symbol.fresh("tk")
    	 	 	  val ek = Symbol.fresh("ek")
    	 	 	  val f = Symbol.fresh("f")
    	 	 	  C.LetK(k, List(r), ctx(r),
    	 	 	    C.LetK(tk, List(), tail(thenE, k) /*nonTail(thenE, v2 => C.AppK(k, List(v2)))*/,
    	 	 	      C.LetK(ek, List(), tail(elseE, k) /*nonTail(elseE, v3 => C.AppK(k, List(v3)))*/,
    	 	 	        /*C.LetL(f, BooleanLit(false),
    	 	 	          nonTail(condE, v1 => C.If(L3Ne, List(v1, f), tk, ek)))*/
    	 	 	    	cond(condE, tk, ek)
    	 	 	    	)))
    	 	 }
    	  }
      }

      case S.Prim(prim, args) => {
    	prim match {
    		case prim: L3TestPrimitive => {
    			nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), ctx)    			
    		}
    		case prim: L3ValuePrimitive => {
    			val v = Symbol.fresh("prim")
    			nonTail_*(args, list => {
    				C.LetP(v, prim, list, ctx(v))
    			})
    		}
    		case _ => throw new Exception("L3TestPrimitive or L3ValuePrimitive expected")
    	}
      }
    }

  private def nonTail_*(trees: List[S.Tree], ctx: List[Symbol]=>C.Tree): C.Tree =
    trees match {
      case List() =>
        ctx(List())
      case t :: ts =>
        nonTail(t, tSym => nonTail_*(ts, tSyms => ctx(tSym :: tSyms)))
    }

  private def tail(tree: S.Tree, k: Symbol): C.Tree = {
    tree match {
      case S.Let((name, value) :: rest, body) =>
        tempLetK("lk", List(name), tail(S.Let(rest, body), k)) { lk =>
          tail(value, lk) }

      case S.Let(List(), body) =>
        tail(body, k)

      case S.App(fun, args) => {
    	  nonTail_*(fun :: args, list => {
    	 	  C.AppF(list.head, k, list.tail)
    	  })
      }

      case S.If(condE, thenE, elseE) => {
    	  condE match {
    	 	  case S.Prim(prim, args) => {
    	 	 	  prim match {
    	 	 	 	  case prim: L3TestPrimitive => {
    	 	 	 		  val r = Symbol.fresh("r")
    	 	 	 		  val tk = Symbol.fresh("tk")
    	 	 	 		  val ek = Symbol.fresh("ek")
    	 	 	 		  C.LetK(tk, List(), tail(thenE, k),
    	 	 	 		  	  C.LetK(ek, List(), tail(elseE, k),
						        nonTail_*(args, list => {
						    	  C.If(prim, list, tk, ek)})))
    	 	 	 	  }
    	 	 	 	  case _ => throw new Exception("L3TestPrimitive expected")
    	 	 	  }
    	 	  }
    	 	  case _ => {
    	 	 	  val r = Symbol.fresh("r")
    	 	 	  val tk = Symbol.fresh("tk")
    	 	 	  val ek = Symbol.fresh("ek")
    	 	 	  val f = Symbol.fresh("f")
    	 	 	    C.LetK(tk, List(), tail(thenE, k),
    	 	 	      C.LetK(ek, List(), tail(elseE, k),
    	 	 	    	cond(condE, tk, ek)))
    	 	 }
    	  }
      }
      
      case S.Prim(prim, args) => {
    	prim match {
    		case prim: L3TestPrimitive => {
    			tail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), k)    			
    		}
    		case prim: L3ValuePrimitive => {
    			val v = Symbol.fresh("prim")
    			nonTail_*(args, list => { C.LetP(v, prim, list, C.AppK(k, List(v))) })
    		}
    		case _ => throw new Exception("L3TestPrimitive or L3ValuePrimitive expected")
    	}
      }
      
      case S.LetRec(functions, body) => {
    	  C.LetF(functions.map (f => {
    		  C.FunDef(f.name, k, f.args, tail(f.body, k))
    	  }), tail(body, k))
      }

      case default => nonTail(tree, v => C.AppK(k, List(v)))
    }
  }

  private def cond(tree: S.Tree, truek: Symbol, falsek: Symbol): C.Tree = {
    tree match {
      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))) =>
        cond(condE, truek, falsek)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(true))) =>
        cond(condE, falsek, truek)

      case S.If(condE, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(true))) =>
        cond(condE, truek, truek)

      case S.If(condE, S.Lit(BooleanLit(false)), S.Lit(BooleanLit(false))) =>
        cond(condE, falsek, falsek)

      case S.If(e1, S.Lit(BooleanLit(true)), e2) => {
    	val ak = Symbol.fresh("ak")
    	//C.LetK(ak, List(), cond(e1, ak, truek), cond(e2, truek, falsek))
    	C.LetK(ak, List(), cond(e2, truek, falsek), cond(e1, truek, ak))
      }

      case S.If(e1, S.Lit(BooleanLit(false)), e2) => {
    	val ak = Symbol.fresh("ak")
    	//C.LetK(ak, List(), cond(e1, ak, falsek), cond(e2, truek, falsek))
    	C.LetK(ak, List(), cond(e2, truek, falsek), cond(e1, truek, ak))
      }

      case S.If(e1, e2, S.Lit(BooleanLit(true))) => {
    	val ak = Symbol.fresh("ak")
    	C.LetK(ak, List(), cond(e2, truek, falsek), cond(e1, ak, truek))
      }
      
      case S.If(e1, e2, S.Lit(BooleanLit(false))) => {
    	val ak = Symbol.fresh("ak")
    	C.LetK(ak, List(), cond(e2, truek, falsek), cond(e1, ak, falsek))
      }

      case S.Prim(p: L3TestPrimitive, args) =>
        nonTail_*(args, as => C.If(p, as, truek, falsek))

      case other =>
        nonTail(other, o =>
          nonTail(S.Lit(BooleanLit(false)), n =>
            C.If(L3Ne, List(o, n), truek, falsek)))
    }
  }
  
  private def tempLetK(kName: String, args: List[C.Name], contBody: C.Tree)(body: C.Name => C.Tree): C.Tree = {
    val kSym = Symbol.fresh(kName)
    C.LetK(kSym, args, contBody, body(kSym))
  }  
}
