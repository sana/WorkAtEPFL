package l3

import BitTwiddling.bitsToIntMSBF
import l3.{ SymbolicCPSTreeModule => H }
import l3.{ SymbolicCPSTreeModuleLow => L }
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

/**
 * Data-representation phase for the CPS language. Translates a tree
 * with high-level datatypes (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level datatypes (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSDataRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    hoist(transform(tree))
	//transform(tree)
  
  private def hoist(tree: L.Tree): L.Tree = {
	tree match {
		case L.LetL(n, l, e) => {
		  hoist(e) match {
		 	  case L.LetF(fs, ep) => L.LetF(fs, L.LetL(n, l, ep))
		 	  case _ => tree
		  }
		}
		
		case L.LetP(name, prim, args, e) => {
			hoist(e) match {
				case L.LetF(fs, ep) => L.LetF(fs, L.LetP(name, prim, args, ep))
				case _ => tree
			}
		}

		case L.LetK(name, args, contBody, body) => hoist(body) match {
			case L.LetF(fs2, ep) => {
				hoist(contBody) match {
					case L.LetF(fs1, bp) => L.LetF(fs1 ++ fs2, L.LetK(name, args, bp, ep))
					case _ => L.LetF(fs2, L.LetK(name, args, contBody, ep))
				}
			}
			case _ => {
				hoist(contBody) match {
					case L.LetF(fs1, bp) => L.LetF(fs1, L.LetK(name, args, bp, body))
					case _ => tree
				}
			}
		}

        case L.LetF(functions, body) => {
        	hoist(body) match {
        		case L.LetF(fs, ep) => L.LetF(_hoistFunctions(functions) ++ fs, ep)
        		case _ => L.LetF(_hoistFunctions(functions), body)
        	}
        }
        
        case x => L.LetF(List(), x)
	}
  }
  
  private def _hoistFunctions(functions: List[L.FunDef]): List[L.FunDef] = {
	functions match {
		case List() => List()
		case x :: xs => x match {
			case L.FunDef(n_i, retK_i, args_i, e_i) => {
				hoist(e_i) match {
        			case L.LetF(fs_i, ep_i) => L.FunDef(n_i, retK_i, args_i, ep_i) :: (fs_i ++ _hoistFunctions(xs))
        			case _ => x :: _hoistFunctions(xs)
        		}
			}
			case _ => throw new Exception("Expected FunDef, but found " + x)
		}
    }
  }

  private def getFreevars(t: H.Tree): Set[H.Name] = t match {
    case H.LetL(name, value, body) => getFreevars(body) - name
    case H.LetP(name, prim, args, body) => (getFreevars(body) - name) ++ args
    case H.LetK(name, args, contBody, body) => getFreevars(body) ++ (getFreevars(contBody) -- (Set() ++ args))
    case H.LetF(functions, body) => (getFreevars(body) ++
      (Set[H.Name]() /: functions)(
        (b0: Set[H.Name], ak: H.FunDef) => ak match {
          case H.FunDef(_, _, args, body) => b0 ++ (getFreevars(body) -- (Set[H.Name]() ++ args))
          case _ => null
        })) -- (Set[H.Name]() ++ functions.map(x =>
        x match {
          case H.FunDef(name, _, _, _) => name
          case _ => null
        }))
    case H.AppK(cont, args) => Set() ++ args
    case H.AppF(fun, retK, args) => (Set() + fun) ++ args
    case H.If(cond, args, thenK, elseK) => Set() ++ args
    case _ => Set()
  }
  
  private def allocateBlocks(names: List[H.Name], ctx: Map[H.Name, Set[H.Name]], rename: Map[H.Name, H.Name], c0: Symbol, body: H.Tree, namesOriginal: List[H.Name]): L.Tree = {
    names match {
      case List() => { configureBlocks(namesOriginal, ctx, rename, c0, body) }
      case x :: xs => {
        ctx.get(x) match {
          case Some(freeVars) => {
            rename.get(x) match {
              case Some(m1) =>
                var fv1 = Symbol.fresh("fv")
                L.LetL(fv1, freeVars.size + 1,
                  L.LetP(names.head, CPSBlockAlloc(202), List(fv1),
                    allocateBlocks(xs, ctx, rename, c0, body, namesOriginal)))
              case None => throw new Exception("Variable name " + x + " not renamed in the context: " + rename)
            }
          }
          case None => throw new Exception("Variable name " + x + " not found in the context: " + ctx)
        }
      }
    }
  }
  
  private def configureFreeVars(x: H.Name, xs: List[H.Name], ctx: Map[H.Name, Set[H.Name]], freeVars: Set[H.Name], rename: Map[H.Name, H.Name], c0: Symbol, body: H.Tree, id: Int): L.Tree = {
	if (freeVars.size == 0)
      configureBlocks(xs, ctx, rename, c0, body)
	else {
	  var t1 = Symbol.fresh("t")
	  var t2 = Symbol.fresh("t")
	  L.LetL(t1, id, L.LetP(t2, CPSBlockSet, List(x, t1, freeVars.head), configureFreeVars(x, xs, ctx, freeVars.tail, rename, c0, body, id + 1)))
	}
  }

  private def configureBlocks(names: List[H.Name], ctx: Map[H.Name, Set[H.Name]], rename: Map[H.Name, H.Name], c0: Symbol, body: H.Tree): L.Tree = {
    names match {
      case List() => transform(body)
      case x :: xs => {
        ctx.get(x) match {
          case Some(freeVars) => {
            rename.get(x) match {
              case Some(m1) =>
                var t1 = Symbol.fresh("t")
                
                L.LetP(t1, CPSBlockSet, List(x, c0, m1),
                  configureFreeVars(x, xs, ctx, freeVars, rename, c0, body, 1))
              case None => throw new Exception("Variable name " + x + " not renamed in the context: " + rename)
            }
          }
          case None => throw new Exception("Variable name " + x + " not found in the context: " + ctx)
        }
      }
    }
  }

  private def computeFreevars(functions: List[H.FunDef]): Map[H.Name, Set[H.Name]] = {
    functions match {
      case List() => new HashMap[H.Name, Set[H.Name]]();
      case x :: xs => {
        x match {
          case H.FunDef(n1, retK1, args1, body1) => computeFreevars(xs) + ((n1, getFreevars(body1) - n1 - retK1 -- args1))
          case _ => throw new Exception("Expected FunDef, but received " + x)
        }
      }
    }
  }
  
  private def unpackFreeVarsCallTransform(env1: Symbol, body1: H.Tree, substMap: Map[H.Name, H.Name], freeVarsMap: Map[H.Name, Int]): L.Tree = {
	  if (freeVarsMap.size == 0)
	    transform(body1).subst(substMap)
	  else {
	 	var t = Symbol.fresh("t")
	 	L.LetL(t, freeVarsMap.head._2,
	 	  L.LetP(freeVarsMap.head._1, CPSBlockGet, List(env1, t),
	 	    unpackFreeVarsCallTransform(env1, body1, substMap, freeVarsMap.tail)))
	  }
  }
  
  private def unpackFreeVars(fv1: Set[H.Name], m1: H.Name, retK1: H.Name, env1: Symbol, args1: List[H.Name], body1: H.Tree, substMap: Map[H.Name, H.Name], freeVarsMap: Map[Symbol, Int], id: Int): L.FunDef = {
	if (fv1.size == 0)
	  L.FunDef(m1, retK1, List(env1) ++ args1, unpackFreeVarsCallTransform(env1, body1, substMap, freeVarsMap))
    else {
      var w1 = Symbol.fresh("w")
      substMap.update(fv1.head, w1)
      freeVarsMap.update(w1,  id)
      unpackFreeVars(fv1.tail, m1, retK1, env1, args1, body1, substMap, freeVarsMap, id + 1)
    }
  }

  private def renameFunctions(functions: List[H.FunDef]): Map[H.Name, H.Name] = {
    functions match {
      case List() => new HashMap[H.Name, H.Name]();
      case x :: xs => {
        x match {
          case H.FunDef(n1, _, _, _) => renameFunctions(functions.tail) + ((n1, Symbol.fresh("m")))
          case _ => throw new Exception("Expected FunDef, but received " + x)
        }
      }
    }
  }

  private def transform(tree: H.Tree): L.Tree = tree match {
    case H.LetL(name, IntLit(value), body) =>
      L.LetL(name, (value << 1) | 1, transform(body))

    case H.LetL(name, BooleanLit(value), body) => {
      value match {
        case true => L.LetL(name, bitsToIntMSBF(List(1, 1, 1, 0)), transform(body))
        case false => L.LetL(name, bitsToIntMSBF(List(0, 1, 1, 0)), transform(body))
      }
    }

    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(List(0, 1, 0)), transform(body))

    case H.LetP(name, prim, args, body) => {
      prim match {
        case L3BlockAlloc(tag) =>
          assert(args.length == 1)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSArithShiftR, args ++ List(c1),
              L.LetP(name, CPSBlockAlloc(tag), List(t1), transform(body))))

        case L3BlockTag =>
          assert(args.length == 1)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          var t2 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSBlockTag, args,
              L.LetP(t2, CPSArithShiftL, List(t1, c1),
                L.LetP(name, CPSAdd, List(t2, c1), transform(body)))))

        case L3BlockLength =>
          assert(args.length == 1)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          var t2 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSBlockSize, args,
              L.LetP(t2, CPSArithShiftL, List(t1, c1),
                L.LetP(name, CPSAdd, List(t2, c1), transform(body)))))

        case L3BlockGet =>
          /* @block-get b n */
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var n = Symbol.fresh("n")
          var v = Symbol.fresh("v")
          var t = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(n, CPSArithShiftR, List(args.tail.head, c1),
              L.LetP(name, CPSBlockGet, List(args.head, n), transform(body))))

        case L3BlockSet =>
          /* @block-set! b n v */
          assert(args.length == 3)

          var c1 = Symbol.fresh("c")
          var n = Symbol.fresh("n")
          var v = Symbol.fresh("v")

          L.LetL(c1, 1,
            L.LetP(n, CPSArithShiftR, List(args.tail.head, c1),
              L.LetP(name, CPSBlockSet, List(args.head, n, args.tail.tail.head), transform(body))))

        case L3IntAdd =>
          // [n + m] = [n] + [m] - 1
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          L.LetL(c1, 1,
            L.LetP(t1, CPSAdd, args,
              L.LetP(name, CPSSub, List(t1, c1), transform(body))))

        case L3IntSub =>
          // [n - m] = [n] - [m] + 1
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          L.LetL(c1, 1,
            L.LetP(t1, CPSSub, args,
              L.LetP(name, CPSAdd, List(t1, c1), transform(body))))

        case L3IntMul =>
          // [n * m] = ([n] - 1) * ([m] >> 1) + 1
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          var n = Symbol.fresh("n")
          var m = Symbol.fresh("m")

          L.LetL(c1, 1,
            L.LetP(n, CPSSub, List(args.head, c1),
              L.LetP(m, CPSArithShiftR, List(args.tail.head, c1),
                L.LetP(t1, CPSMul, List(n, m),
                  L.LetP(name, CPSAdd, List(t1, c1), transform(body))))))
                  
        case L3IntDiv =>
          // [n / m] = 2 * ([n] - 1) / ([m] - 1) + 1
          // [n / m] = ([n] - 1) / ([m] >> 1) | 1
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var t2 = Symbol.fresh("t")
          var n = Symbol.fresh("n")
          var m = Symbol.fresh("m")

          L.LetL(c1, 1,
            L.LetP(n, CPSSub, List(args.head, c1),
              L.LetP(m, CPSArithShiftR, List(args.tail.head, c1),
                L.LetP(t2, CPSDiv, List(n, m),
                  L.LetP(name, CPSOr, List(t2, c1), transform(body))))))

        case L3IntMod =>
          // [n % m] = ((n - 1)) % ((m - 1)) + 1
          assert(args.length == 2)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          var n = Symbol.fresh("n")
          var m = Symbol.fresh("m")

          L.LetL(c1, 1,
            L.LetP(n, CPSSub, List(args.head, c1),
              L.LetP(m, CPSSub, List(args.tail.head, c1),
                L.LetP(t1, CPSMod, List(n, m),
                  L.LetP(name, CPSAdd, List(t1, c1), transform(body))))))

        case L3CharRead =>
          assert(args.length == 0)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")
          var t2 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSCharRead, List(),
              L.LetP(t2, CPSArithShiftL, List(t1, c1),
                L.LetP(name, CPSAdd, List(t2, c1), transform(body)))))

        case L3CharPrint =>
          assert(args.length == 1)

          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSArithShiftR, List(args.head, c1),
              L.LetP(name, CPSCharPrint, List(t1), transform(body))))

      }
    }

    case H.LetK(name, args, contBody, body) =>
      L.LetK(name, args, transform(contBody), transform(body))

    case H.LetF(functions, body) =>
      /* Variable names */
      var names: List[H.Name] = functions.map(x =>
        x match {
          case H.FunDef(n1, _, _, _) => n1
          case _ => throw new Exception("Expected FunDef, but received " + x)
        })

      /* Map between a variable name and the free variables from its body */
      var ctx: Map[H.Name, Set[H.Name]] = computeFreevars(functions);

      var rename: Map[H.Name, H.Name] = renameFunctions(functions)
      var gc0 = Symbol.fresh("c")

      L.LetF(functions.map(x =>
        x match {
          case H.FunDef(n1, retK1, args1, body1) =>
            var c1 = Symbol.fresh("c")
            var env1 = Symbol.fresh("env")
            var substMap: Map[H.Name, H.Name] = new HashMap[H.Name, H.Name]

            substMap.update(n1, env1)
            rename.get(n1) match {
              case Some(m1) =>
                ctx.get(n1) match {
                  case Some(fv1) => unpackFreeVars(fv1, m1, retK1, env1, args1, body1, substMap, new HashMap[Symbol, Int], 1)
                  case None => throw new Exception("Free variable not computed for " + n1)
                }
              case None => throw new Exception("Variable name " + x + " not renamed in the context: " + rename)
            }
          case _ => null
        }), L.LetL(gc0, 0, allocateBlocks(names, ctx, rename, gc0, body, names)))

    case H.AppK(cont, args) =>
      L.AppK(cont, args)

    case H.AppF(fun, retK, args) => {
      var c = Symbol.fresh("c")
      var f = Symbol.fresh("f")
      L.LetL(c, bitsToIntMSBF(List(0)),
        L.LetP(f, CPSBlockGet, List(fun, c), L.AppF(f, retK, List(fun) ++ args)))
    }

    case H.If(cond, args, thenK, elseK) => {
      cond match {
        case L3IntLt => L.If(CPSLt, args, thenK, elseK)
        case L3IntLe => L.If(CPSLe, args, thenK, elseK)
        case L3IntGe => L.If(CPSGe, args, thenK, elseK)
        case L3IntGt => L.If(CPSGt, args, thenK, elseK)
        case L3Eq => L.If(CPSEq, args, thenK, elseK)
        case L3Ne => L.If(CPSNe, args, thenK, elseK)

        case L3IntP => {
          assert(args.length == 1)
          var c1 = Symbol.fresh("c")
          var t1 = Symbol.fresh("t")

          L.LetL(c1, 1,
            L.LetP(t1, CPSAnd, List(args.head, c1),
              L.If(CPSEq, List(t1, c1), thenK, elseK)))
        }

        case L3FunctionP => {
          var k = Symbol.fresh("k")
          var m = Symbol.fresh("m")
          var t = Symbol.fresh("t")
          var r = Symbol.fresh("r")
          var t1 = Symbol.fresh("t")
          var t2 = Symbol.fresh("t")

          L.LetK(k, List(),
            L.LetL(t1, 202, L.LetP(t2, CPSBlockTag, List(args.head),
              L.If(CPSEq, List(t1, t2), thenK, elseK))),

            L.LetL(m, bitsToIntMSBF(List(1, 1)),
              L.LetL(t, bitsToIntMSBF(List(0, 0)),
                L.LetP(r, CPSAnd, List(args.head, m),
                  L.If(CPSEq, List(r, t), k, elseK)))))
        }

        case L3BlockP => {
          assert(args.length == 1)

          var m = Symbol.fresh("m")
          var t = Symbol.fresh("t")
          var r = Symbol.fresh("r")

          L.LetL(m, bitsToIntMSBF(List(1, 1)),
            L.LetL(t, bitsToIntMSBF(List(0, 0)),
              L.LetP(r, CPSAnd, List(args.head, m),
                L.If(CPSEq, List(r, t), thenK, elseK))))
        }

        case L3BoolP => {
          assert(args.length == 1)

          var m = Symbol.fresh("m")
          var t = Symbol.fresh("t")
          var r = Symbol.fresh("r")

          L.LetL(m, bitsToIntMSBF(List(1, 1, 1)),
            L.LetL(t, bitsToIntMSBF(List(1, 1, 0)),
              L.LetP(r, CPSAnd, List(args.head, m),
                L.If(CPSEq, List(r, t), thenK, elseK))))
        }

        case L3UnitP => {
          assert(args.length == 1)

          var m = Symbol.fresh("m")
          var t = Symbol.fresh("t")
          var r = Symbol.fresh("r")

          L.LetL(m, bitsToIntMSBF(List(1, 1, 1)),
            L.LetL(t, bitsToIntMSBF(List(0, 1, 0)),
              L.LetP(r, CPSAnd, List(args.head, m),
                L.If(CPSEq, List(r, t), thenK, elseK))))
        }
      }
    }

    case H.Halt => L.Halt
  }

  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: List[L.Name])(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tK if it
   * is the case, and eK otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: List[Int], tK: L.Name, eK: L.Name): L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 })) { mask =>
      tempLetP(CPSAnd, List(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits)) { value =>
          L.If(CPSEq, List(masked, value), tK, eK)
        }
      }
    }

  /**
   * Generate an If tree to check whether the value bound to the given
   * name is a block tagged with the given tag. The generated If tree
   * will apply continuation tK if it is the case, and eK otherwise.
   */
  private def ifTaggedBlock(arg: L.Name, tag: BlockTag.Value, tK: L.Name, eK: L.Name): L.Tree = {
    val checkTagK = Symbol.fresh("k")
    L.LetK(checkTagK, List(),
      tempLetL(tag.id) { tag =>
        tempLetP(CPSBlockTag, List(arg)) { blockTag =>
          L.If(CPSEq, List(tag, blockTag), tK, eK)
        }
      },
      ifEqLSB(arg, List(0, 0), checkTagK, eK))
  }
}
