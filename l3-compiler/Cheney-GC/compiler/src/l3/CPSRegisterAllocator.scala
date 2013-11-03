package l3

import collection.mutable.{ Map => MutableMap }
import l3.{ SymbolicCPSTreeModuleLow => S }
import l3.{ RegisterCPSTreeModule => R }

/**
 * A simple register allocator for CPS/L₃.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */
object CPSRegisterAllocator extends (S.Tree => R.Tree) {
  // Calling conventions:
  //   I0      contains caller's Lb
  //   I1      contains caller's Ib
  //   I2      contains return address
  //   I3..I15 contain arguments 1 to 13
  //   I3/O3   contains return value
  //   Ob, Lb  are initially zero

  def apply(tree: S.Tree): R.Tree =
    transformClosed(tree, Map.empty, Map.empty)

  private type RegMap = Map[S.Name, R.Reg]
  private type KRegMap = Map[S.Name, Seq[R.Reg]]

  private def transformClosed(closedTree: S.Tree, r: RegMap, kr: KRegMap): R.Tree = {
    val I2 = ASMRegisterFile.in(2)
    val I3 = ASMRegisterFile.in(3)
    val O3 = ASMRegisterFile.out(3)
    val freeVars = freeVarsMap(closedTree)
    val isRetCont = retConts(closedTree)

    def transform(tree: S.Tree, r: RegMap, kr: KRegMap): R.Tree = {
      def rOrL(n: S.Name): R.Name = r.getOrElse(n, R.Label(n))
      val withReg = withReg0(_ : S.Name, r, freeVars(tree)) _
      val withRegs = withRegs0(_ : List[S.Name], r, freeVars(tree)) _

      tree match {
        case S.LetL(name, value, body) =>
          val rName = freeReg(r, freeVars(body))
          R.LetL(rName, value,
                 transform(body, r + (name -> rName), kr))

        case S.LetP(name, prim, args, body) =>
          withRegs(args) { (rArgs, r) =>
            val rName = freeReg(r, freeVars(body))
            R.LetP(rName, prim, rArgs,
                   transform(body, r + (name -> rName), kr)) }

        case S.LetK(name, args, contBody, body) if isRetCont(name) =>
          assume(args.length == 1)
          val rArg0 = R.Reg(O3)
          val rArgs0 = List(rArg0)
          val rArgs = freeRegs(r, freeVars(contBody), 1).toList
          val contBodyT = R.LetP(rArgs(0), CPSCopy, List(rArg0),
                                 transform(contBody, r ++ (args zip rArgs), kr))
          R.LetK(R.Label(name), rArgs0, contBodyT,
                 transform(body, r, kr + (name -> rArgs0)))

        case S.LetK(name, args, contBody, body) =>
          val rArgs = freeRegs(r, freeVars(contBody), args.length).toList
          val contBodyT = transform(contBody, r ++ (args zip rArgs), kr)
          R.LetK(R.Label(name), rArgs, contBodyT,
                 transform(body, r, kr + (name -> rArgs)))

        case S.LetF(funs, body) =>
          R.LetF(funs map transformFun,
                 transform(body, r, kr))

        case S.AppK(cont, args) =>
          val rOutArgs = kr(cont).toList
          withRegs(args) { (rArgs, _) =>
            parLet(rOutArgs zip rArgs,
                   R.AppK(rOrL(cont), rOutArgs)) }

        case S.AppF(fun, retK, args) =>
          val rOutArgs = ccOutRegs(args.length)
          withRegs(fun :: args) { case (rFun :: rArgs, _) =>
            parLet(rOutArgs zip rArgs,
                   R.AppF(rFun, rOrL(retK), rOutArgs)) }

        case S.If(cond, args, thenK, elseK) =>
          R.If(cond, args map r, rOrL(thenK), rOrL(elseK))

        case S.Halt =>
          R.Halt
      }
    }

    def transformFun(fun: S.FunDef): R.FunDef = {
      val funL = R.Label(fun.name)
      val rArgs = ccInRegs(fun.args.length)
      val allocatedRegs = (fun.retK, R.Reg(I2)) :: (fun.args zip rArgs)
      R.FunDef(funL, R.Reg(I2), rArgs,
               transformClosed(fun.body,
                               Map(allocatedRegs : _*),
                               Map(fun.retK -> Seq(R.Reg(I3)))))
    }

    transform(closedTree, r, kr)
  }

  private def freeRegs(r: RegMap, freeVars: Set[S.Name], count: Int): Seq[R.Reg] = {
    val usedRegs = freeVars flatMap { (r andThen { _.reg }).lift(_) }
    ASMRegisterFile.local filterNot usedRegs  take count  map R.Reg
  }

  private def freeReg(r: RegMap, freeVars: Set[S.Name]) =
    freeRegs(r, freeVars, 1).head

  private def withReg0(name: S.Name, r: RegMap, freeVars: Set[S.Name])
                      (body: (R.Reg, RegMap) => R.Tree): R.Tree =
     (r get name) match {
       case Some(reg) =>
         body(reg, r)
       case None =>
         val reg = freeReg(r, freeVars)
         R.LetP(reg, CPSCopy, List(R.Label(name)),
                body(reg, r + (name -> reg)))
     }

  private def withRegs0(names: List[S.Name], r: RegMap, freeVars: Set[S.Name])
                       (body: (List[R.Reg], RegMap) => R.Tree): R.Tree =
    names match {
      case List() =>
        body(List(), r)
      case n1 :: ns =>
        withReg0(n1, r, freeVars) { (rN1, r1) =>
          withRegs0(ns, r1, freeVars) { (rNs, r2) => body(rN1 :: rNs, r2) } }
    }

  private def parLet(bdgs: Seq[(R.Name, R.Name)], body: R.Tree): R.Tree =
    (bdgs :\ body) { case ((to, from), body) =>
      R.LetP(to, CPSCopy, List(from), body) }

  private def freeVarsMap(tree: S.Tree): (S.Tree => Set[S.Name]) = {
    val fvMap = MutableMap[S.Tree, Set[S.Name]]()

    val kEnv = MutableMap[S.Name, Set[S.Name]]()
    def contFV(k: S.Name): Set[S.Name] = kEnv.getOrElse(k, Set.empty)

    def wrapper(tree: S.Tree): Set[S.Name] =
      fvMap.getOrElseUpdate(tree, worker(tree))

    def worker(tree: S.Tree): Set[S.Name] = tree match {
      case S.LetL(name, _, body) =>
        wrapper(body) - name
      case S.LetP(name, _, args, body) =>
        args.toSet | (wrapper(body) - name)
      case S.LetK(name, args, cBody, body) =>
        kEnv += name -> wrapper(cBody)
        wrapper(body) &~ args.toSet
      case S.LetF(_, body) =>
        wrapper(body)
      case S.AppK(cont, args) =>
        args.toSet | contFV(cont)
      case S.AppF(fun, retK, args) =>
        Set(fun) | contFV(retK) | args.toSet
      case S.If(_, args, thenK, elseK) =>
        args.toSet | contFV(thenK) | contFV(elseK)
      case S.Halt =>
        Set.empty
    }

    wrapper(tree)
    fvMap
  }

  private def retConts(tree: S.Tree): Set[S.Name] = tree match {
    case S.LetL(_, _, body) => retConts(body)
    case S.LetP(_, _, _, body) => retConts(body)
    case S.LetK(_, _, cBody, body) => retConts(cBody) | retConts(body)
    case S.LetF(_, body) => retConts(body)
    case S.AppK(_, _) => Set.empty
    case S.AppF(_, retK, _) => Set(retK)
    case S.If(_, _, _, _) => Set.empty
    case S.Halt => Set.empty
  }

  private def ccInRegs(n: Int): List[R.Reg] =
    (ASMRegisterFile.in drop 3 take n map R.Reg).toList

  private def ccOutRegs(n: Int): List[R.Reg] =
    (ASMRegisterFile.out drop 3 take n map R.Reg).toList
}
