package scala.tasty.internal
package convert

trait TreeConverter {
  self: API =>

  import self.GlobalToTName._
  import self.ast.{ tpd => t }
  import self.{ Constants => tc }

  private[convert] val scopeStack = scala.collection.mutable.Stack[g.Symbol]()
  def addToScope(tree: g.Tree)(fn: => t.Tree): t.Tree = {
    tree match {
      case _: g.DefTree =>
        addToScope(tree.symbol)(fn)
      case _ => fn
    }
  }
  def addToScope(symbol: g.Symbol)(fn: => t.Tree): t.Tree = {
    scopeStack push symbol
    val res = fn
    scopeStack.pop
    res
  }
  def currentScope = scopeStack.headOption match {
    case Some(scope) => scope
    case None => g.NoSymbol
  }

  //TODO code alignment is required
  def convertTrees(tree: List[g.Tree]): List[t.Tree] = {
    val convertedList = tree map convertTree
    import scala.collection.mutable.ArrayBuffer
    var resList: ArrayBuffer[t.Tree] = ArrayBuffer() 
    convertedList foreach {
      case th: t.Thicket => resList ++= th.trees
      case tr => resList += tr
    }
    resList.toList
  }

  def convertSelect(qual: g.Tree, name: g.Name, tp: g.Type): t.Select = {
    val tTpe = convertType(tp)
    val tQual = convertTree(qual)
    convertSelect(tQual, name, tTpe)
  }

  def convertSelect(tQual: t.Tree, name: dotc.core.Names.Name, tTpe: self.Types.Type): t.Select = {
    t.Select(tQual, name) withType tTpe
  }

  def getTemplateTpe(clsSym: self.Symbols.Symbol, implSym: g.Symbol) = {
    val dummySymbol = newLocalDummy(clsSym, clsSym.coord, implSym)
    getTermRef(dummySymbol)
  }

  def processClassTypeParams(typeParams: List[g.Symbol], base: self.Symbols.Symbol): List[t.TypeDef] = {
    typeParams.map {
      typeParam =>
        val isExpandedSymbol = isExpandedSym(typeParam)
        val tSym = convertSymbol(typeParam)
        val tTypeParam = tSym.typeRef
        val tRhs = t.TypeTree(convertType(typeParam.tpe.bounds))
        val tBase = base
        val firstName = if (isExpandedSymbol) self.expandedName(tBase, typeParam.name).toTypeName else convertToTypeName(typeParam.name)
        val t1 = t.TypeDef(firstName, tRhs) withType tTypeParam
        if (isExpandedSymbol) {
          import dotc.core._
          val tOwnerSym = convertSymbol(typeParam.owner)
          val secondTypeDefSym = newSymbol(tOwnerSym, convertToTypeName(typeParam.name), Flags.PrivateLocal | Flags.Synthetic, g.NoSymbol)
          val secondTypeDefTpe = secondTypeDefSym.typeRef
          val secondRhsTpe = self.Types.TypeAlias(tTypeParam) withGType typeParam.tpe //convertTypeAlias(typeParam.tpe)
          val t2 = t.TypeDef(convertToTypeName(typeParam.name), t.TypeTree(secondRhsTpe)) withType secondTypeDefTpe
          List(t1, t2)
        } else List(t1)
    }.flatten
  }

  def processClassTypeParams(typeParams: List[g.Symbol], base: g.Symbol): List[t.TypeDef] = {
    val convBase = convertSymbol(base)
    processClassTypeParams(typeParams, convBase)
  }

  def convertTree(tree: g.Tree): t.Tree = {
    //println(s"tree: ${g.showRaw(tree)}")
    addToScope(tree) {
      val resTree = tree match {
        case g.Ident(name) =>
          val identSymbol = tree.symbol
          val tIdentSymbol = convertSymbol(identSymbol)
          //TODO check the cases when termRef is the tpe for Ident
          val tTpe = if ((identSymbol ne null) && identSymbol.isTerm) {
        	// references for param accessors from the same class should
        	// be of view t.TermRef(t.Ident, name)
            getIdentType(identSymbol)
          } else {
            convertType(tree.tpe)
          }
          t.Ident(name) withType tTpe
        case th @ g.This(qual) =>
          //TODO - if there will be problems - try to add - if (th.symbol.isClass && !th.symbol.isPackageClass)
          val tTpe = getThisType(th.symbol)
          t.This(qual) withType tTpe
        case s@g.Select(qual, name) =>
          //TODO - persist tpe from symbol, not from tree
          if (s.symbol.isConstructor) {
            convertSelect(qual, name, s.symbol.info)
          } else convertSelect(qual, name, tree.tpe) 
        case apply: g.Apply =>
          val g.treeInfo.Applied(fun, targs, argss) = apply
          fun match {
            // synthetic $isInstanceOf should be rewritten with isInstanceOf
            case fun @ g.Select(_, selName) if specialCases.contains(selName) =>
              t.TypeApply(convertTree(fun), convertTrees(targs))
            case _ =>
              val tArgss = argss map convertTrees
              val tFun0 = convertTree(fun)
              val tFun = fun match {
            	//constructor invocation new Test(x)(y)
                //type arguments should be added to constructor invocation (if class/trait has type parameters)
                case sel: g.Select if sel.symbol.isConstructor && sel.qualifier.tpe.typeArgs.nonEmpty =>
                  val tTypeArgs = sel.qualifier.tpe.typeArgs map { tp => t.TypeTree(convertType(tp)) }
                  t.TypeApply(tFun0, tTypeArgs)
                case _ if targs.nonEmpty => 
                  val tTargs = convertTrees(targs)
                  t.TypeApply(tFun0, tTargs)
                case _ =>
                  tFun0
              }
              tArgss.foldLeft(tFun)(t.Apply(_,_))
          }
        case g.TypeApply(fun, args) =>
          val tFun = convertTree(fun)
          val tArgs = convertTrees(args)
          t.TypeApply(tFun, tArgs)
        case g.Literal(const1) =>
          val tConst = convertConstant(const1)
          t.Literal(tConst) withType tConst.tpe
        case s@g.Super(qual, mix) =>
          val tQual = convertTree(qual)
          //TODO - check inConstrCall
          val tSupType = convertType(s.tpe)
          t.Super(tQual, mix, inConstrCall = false) withType tSupType
        case g.New(tpt) =>
          val tTpt = convertTree(tpt)
          t.New(tTpt)
        case g.Typed(expr, tpt) =>
          val tExpr = convertTree(expr)
          val tTpt = convertTree(tpt)
          t.Typed(tExpr, tTpt)
        case g.Assign(lhs, rhs) =>
          val tLhs = convertTree(lhs)
          val tRhs = convertTree(rhs)
          t.Assign(tLhs, tRhs)
        case g.Block(stats, expr) =>
          expr match {
            case func: g.Function if stats.isEmpty => convertTree(func)
            case _ =>
              val tStats = convertTrees(stats)
              val tExpr = convertTree(expr)
              t.Block(tStats, tExpr)
          }
        case g.If(cond, thenp, elsep) =>
          val tCond = convertTree(cond)
          val tThenp = convertTree(thenp)
          val tElsep = convertTree(elsep)
          t.If(tCond, tThenp, tElsep)
        case g.Match(selector, cases) =>
          val tSelector = convertTree(selector)
          //TODO fix asInstanceOf
          val tCases = convertTrees(cases) map (_.asInstanceOf[t.CaseDef])
          t.Match(tSelector, tCases)
        case g.CaseDef(pat, guard, rhs) =>
          val tPat = convertTree(pat)
          val tGuard = convertTree(guard)
          val tRhs = convertTree(rhs)
          t.CaseDef(tPat, tGuard, tRhs)
        case g.Try(block, cases, finalizer) =>
          val tBlock = convertTree(block)
          val tCases = convertTrees(cases) map (_.asInstanceOf[t.CaseDef])
          val tFinalizer = convertTree(finalizer)
          t.Try(tBlock, tCases, tFinalizer)
        case tt @ g.TypeTree() =>
          //TODO - do we need to persist tt.original?
          //if (tt.original != null) {
          //  val orig = convertTree(tt.original)
          //  t.TypeTree(orig)
          //} else {
          val tastyType = convertType(tt.tpe)
          t.TypeTree(tastyType)
          //}
        case b @ g.Bind(name, body) =>
          val tBody = convertTree(body)
          val tName = convertToTermName(name)
          val tTpe = convertSymbol(b.symbol).termRef
          t.Bind(tName, tBody) withType tTpe
        case g.Alternative(alts) =>
          val tAlts = convertTrees(alts)
          t.Alternative(tAlts)
        case tree @ g.ValDef(mods, name, tpt, rhs) =>
          val valTp = tree match {
            // Special case - primary constr parameters symbols should be persisted but all
            // next references to them should be changed to param accessor
            // Currently symbol for parameter of primary constr is not cached
            case _ if isPrimaryConstrParameter(tree.symbol) =>
              val tSymbol = convertSymImpl(tree.symbol, directParam = true)
              getTermRef(tSymbol)
            case _ =>
              getTermRef(tree.symbol)
          }
          val tTpt = convertTree(tpt).asInstanceOf[t.TypeTree]
          val tRhs = convertTree(rhs)
          t.ValDef(name, tTpt, tRhs) withType valTp
        case tree @ g.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val defTp = getTermRef(tree.symbol)

          val tTparams = {
            if (tree.symbol.isConstructor) processClassTypeParams(tree.symbol.owner.typeParams, tree.symbol)
            else convertTrees(tparams).asInstanceOf[List[t.TypeDef]]
          }
          val tVparamss = (vparamss map convertTrees).asInstanceOf[List[List[t.ValDef]]]
          val tTpt =
            if (tree.symbol.isConstructor) t.TypeTree withType convertType(g.definitions.UnitTpe)
            else convertTree(tpt)
          val tRhs =
            rhs match {
              //Remove super invocation from primary constructor
              case g.Block(g.treeInfo.Applied(g.Select(g.Super(_, _), _), _, _) :: stats, expr) if tree.symbol.isPrimaryConstructor =>
                if (stats.nonEmpty) {
                  val tStats = convertTrees(stats)
                  val tExpr = convertTree(expr)
                  t.Block(tStats, tExpr)
                } else {
                  t.EmptyTree
                }
              //Change empty block to EmptyTree in constructor
              case g.Block(Nil, g.Literal(g.Constant(()))) if tree.symbol.isConstructor => t.EmptyTree
              //Add Type parameters application to constructors this(x)(y)...
              case g.Block((g.treeInfo.Applied(sel, _, argss) :: other), expr) if tree.symbol.isConstructor && !tree.symbol.isPrimaryConstructor && tree.symbol.owner.typeParams.nonEmpty =>
                val tSel = convertTree(sel)
                val tArgss = argss map convertTrees
                /* For Dotty in some cases type params can be computed:
                   class Test[T1, T2](x: T1, z: T2) {
                     def this(x: T1) = this(x, "gg")
                   }
                   result: this[T1, String](x, "gg")
                */
                val tTypeParams = tree.symbol.owner.typeParams map { tp => t.TypeTree(convertType(g.definitions.NothingTpe)) }
                val tTypeApply = t.TypeApply(tSel, tTypeParams)
                val tApply = tArgss.foldLeft[t.Tree](tTypeApply)(t.Apply(_, _))

                val tStats = tApply :: convertTrees(other)
                val tExpr = convertTree(expr)
                t.Block(tStats, tExpr)
              case _ => convertTree(rhs)
            }
          t.DefDef(name, tTparams, tVparamss, tTpt, tRhs) withType (defTp)
        case fun @ g.Function(vparams, body) => 
          val tparams: List[t.TypeDef] = Nil
          val vparamss = List(convertTrees(vparams) map {_.asInstanceOf[t.ValDef]})
          val tTpt = t.TypeTree(convertType(body.tpe))
          val tRhs = convertTree(body)
          val defTp = convertSymbol(fun.symbol).termRef
          val dd = t.DefDef(convertToTermName(fun.symbol.name), tparams, vparamss, tTpt, tRhs) withType defTp
          val tIdent = t.Ident(dd.name) withType defTp
          val clTpe = convertType(fun.tpe)
          val cl = t.Closure(List(), tIdent, t.EmptyTree) withType clTpe
          t.Block(List(dd), cl)
        case tree @ g.TypeDef(mods, name, tparams, rhs) =>
          val tTparams = convertTrees(tparams).asInstanceOf[List[t.TypeDef]]

          val rhsTp = rhs match {
            case _: g.Template =>
              getTemplateTpe(convertSymbol(tree.symbol), rhs.symbol)
            case _ if tree.symbol.isAliasType =>
              convertTypeAlias(rhs.tpe)
            case _ =>
              convertType(rhs.tpe)
          }
          val tTree = convertTree(rhs)
          //TODO - fix: rhsTp overrides already computed tpt in convertTree
          val tRhs = tTree withType rhsTp

          val tp = tree.symbol.tpe
          val convertedType = convertType(tp)
          t.TypeDef(name, tRhs) withType convertedType
        case tree @ g.ClassDef(mods, name, tparams, impl) =>
          val clsSym = convertSymbol(tree.symbol)
          val dummyTpe = getTemplateTpe(clsSym, impl.symbol)
          val tImpl = convertTree(impl) withType dummyTpe

          //TODO - in Dotty typedef.tpe is TypeDef(pre, sym)
          val tp = tree.symbol.tpe.typeConstructor
          val convertedType = convertType(tp)
          //TODO tparams should be processed
          t.ClassDef(name, tImpl) withType (convertedType)
        case tree @ g.ModuleDef(_, name, impl) =>
          val modClSym = tree.symbol.moduleClass
          val tModClSym = convertSymbol(modClSym)
          val modClSymTpe = modClSym.tpe
          //generate type with synthetic name (should be ..$)
          //add method to change name/add modifiers
          val tModClSymTpe = convertType(modClSymTpe)
          val modSym = tree.symbol

          //typeDef
          val synthName = syntheticName(modClSym.name).toTypeName

          val dummyTpe = getTemplateTpe(tModClSym, impl.symbol)
          val tImpl = convertTree(impl) withType dummyTpe
          //TODO - be careful with this type, it's better to create type with converted sym
          val tp = tree.symbol.tpe
          val convertedType = convertType(tp)
          //TODO - check constructor type
          val genTypeDef = t.TypeDef(convertToTypeName(synthName), tImpl) withType tModClSymTpe

          //valDef
          import dotc.core.StdNames.nme
          //val tIdent = t.Ident(synthName) withType tModClSymTpe
          val tNew = t.New(tModClSymTpe)
          val tSelectTpe = getTermRef(convertSymbol(modClSymTpe.member(g.nme.CONSTRUCTOR)))
          val tSelect = convertSelect(tNew, nme.CONSTRUCTOR, tSelectTpe)

          val genVDRhs = t.Apply(tSelect, List())
          val genValDef = t.ValDef(name.toTermName, t.TypeTree(tModClSymTpe), genVDRhs) withType getTermRef(modSym)

          t.Thicket(List(genValDef, genTypeDef))
        case tree @ g.Template(parents, selftree, body) =>
          val (params, rest) = tree.body partition {
            case stat: g.TypeDef => stat.symbol.isParameter
            case stat: g.ValOrDefDef =>
              stat.symbol.isParamAccessor && !stat.symbol.isSetter
            case _ => false
          }
          //emulate dotty style of parents representation (for pickling)
          val primaryCtr = g.treeInfo.firstConstructor(body)
          //if currently processing Def is trait
          val isTrait = tree.symbol.owner.isTrait

          val ap: Option[g.Apply] = primaryCtr match {
            case g.DefDef(_, _, _, _, _, g.Block(ctBody, _)) =>
              ctBody collectFirst {
                case apply: g.Apply => apply
              }
            case _ => None
          }
          val constrArgss: List[List[g.Tree]] = ap match {
            case Some(g.treeInfo.Applied(_, _, argss)) => argss
            case _                                     => Nil
          }
          //def isDefaultAnyRef(tree: g.Tree) = tree match {
          //  case g.Select(g.Ident(sc), name) if name == g.tpnme.AnyRef && sc == g.nme.scala_ => true
          //  case g.TypeTree() => tree.tpe =:= global.definitions.AnyRefTpe
          //  case _ => false
          //}

          //lang.Object => (new lang.Object()).<init> 
          //Apply(Select(New(TypeTree[tpe]), <init>), args)
          val tParents = (tree.parents.zipWithIndex) map {
            //case for parent of class which is a class
            case (gParent, index) if !gParent.symbol.isTrait && !isTrait =>
              val gArgs = constrArgss(index)
              val gParentTpe = gParent.tpe
              val gParentConstructorTpe = gParent.tpe.member(g.nme.CONSTRUCTOR).tpe
              val tParentConstructorTpe = convertType(gParentConstructorTpe)

              val tNewTpe = convertType(gParentTpe.typeConstructor)
              val tNew = t.New(tNewTpe)

              val tSelect = convertSelect(tNew, dotc.core.StdNames.nme.CONSTRUCTOR, tParentConstructorTpe)
              val args = convertTrees(gArgs)

              val typeArgs = gParent.tpe.typeArgs
              val fn = if (typeArgs.nonEmpty) {
                val tTypeArgs = convertTypes(typeArgs) map {t.TypeTree(_)}
                t.TypeApply(tSelect, tTypeArgs)
              } else tSelect

              t.Apply(fn, args)
            //case for all parents of trait and parent of class which is a trait 
            case (gParent, _) =>
              val parentType = convertType(gParent.tpe)
              t.TypeTree(parentType)
          }
          //TODO tParents should be fixed
          //val tParents = convertTrees(parents)

          val tPrimaryCtr = convertTree(primaryCtr)
          val tSelf =
            if (selftree.symbol != g.NoSymbol) {
              val convSelfType = convertType(selftree.symbol.tpe)
              convertTree(selftree).asInstanceOf[t.ValDef] withType (convSelfType)
            } else if (tree.symbol.owner.isModuleClass) {
              val termRef = getTermRef(tree.symbol.owner.companionModule)
              t.ValDef(dotc.core.StdNames.nme.USCOREkw, t.TypeTree(termRef), t.EmptyTree) withType self.Types.NoType
            } else t.EmptyValDef
          val typeParams = tree.symbol.owner.typeParams
          val resTPrimaryCtr = {
            tPrimaryCtr match {
              case dd: t.DefDef => dd
              //TODO - fix to correct constructor representation
              case t.EmptyTree =>
                //TODO - move to standalone method (default constructor tree creation)

                val clsSym = convertSymbol(tree.symbol.owner).asClass
                val constrSymbol = tree.symbol.owner.newConstructor(g.NoPosition) //g.Symbol(g.nme.CONSTRUCTOR)
                val dcSym = newDefaultConstructor(clsSym)
                val dcType = getTermRef(dcSym)
                val unitTpe = convertType(g.definitions.UnitTpe)
                addToScope(constrSymbol) {
                  val tparams = processClassTypeParams(typeParams, dcSym)
                  t.DefDef(dotc.core.StdNames.nme.CONSTRUCTOR, tparams, List(Nil), t.TypeTree(unitTpe), t.EmptyTree) withType dcType
                }.asInstanceOf[t.DefDef]
              case _ => throw new Exception("Not correct constructor is found!")
            }
          }
          //Index out of bounds exception occurs here if invoke typeParams(0) for example:
          //class Test[X <: L]
          val tBody = rest match {
            case constr :: tail if constr.nonEmpty && constr.symbol.isPrimaryConstructor =>
              //TODO - check order of typeParams
              val tDefs = processClassTypeParams(typeParams, tree.symbol.owner)
              tDefs ::: convertTrees(params ::: tail)
            case _ if body.nonEmpty || /*TODO - check: */ typeParams.nonEmpty =>
              //TODO check order of typeParams
              val tDefs = processClassTypeParams(typeParams, tree.symbol.owner)
              tDefs ::: convertTrees(body)
            case _ =>
              List(t.EmptyTree)
          }
          t.Template(resTPrimaryCtr, tParents, tSelf, tBody)
        case g.Import(expr, selectors) =>
          val tExpr = convertTree(expr)
          val tSelectors = convertSelectors(selectors)
          t.Import(tExpr, tSelectors)
        case g.PackageDef(pid, stats) =>
          val tp = pid.tpe
          val tTp = convertType(tp)
          val tPid = convertTree(pid).asInstanceOf[t.RefTree] withType (tTp)
          val tStats = convertTrees(stats)
          t.PackageDef(tPid, tStats) withType (tTp)
        case g.EmptyTree   => t.EmptyTree
        case thr @ g.Throw(expr) =>
          //TODO - fix problem with Throw unpickling inside FromTasty
          val throwName = dotc.core.StdNames.nme.throw_
          val prefixSym = newPackageSymbol(convertSymbol(g.RootClass), g.newTermName("<special-ops>"), dotc.core.Flags.Package, g.NoSymbol)
          val throwIdentSym = newSymbol(prefixSym, throwName, dotc.core.Flags.Method, g.NoSymbol)
          //TODO unify type creation with explicit signature
          //generic symbol representing throw
          //pickleNameAndSig, name: throw
          //params: List(java.lang.Throwable)
          //result: scala.Nothing
          val throwIdentTpe = self.Types.TermRef.withSig(prefixSym.termRef, throwName,
            //TODO - rewrite name creation
            self.Signature(List(g.newTypeName("java.lang.Throwable")), g.newTypeName("scala.Nothing")))
          throwIdentTpe setSym throwIdentSym
          val throwIdent = t.Ident(throwName) withType throwIdentTpe
          val args = List(convertTree(expr))
          t.Apply(throwIdent, args)
        case ld @ g.LabelDef(name, vparams, rhs) =>
          if (name.startsWith(g.nme.WHILE_PREFIX) || name.startsWith(g.nme.DO_WHILE_PREFIX)) {
            //TODO - generalize DefDef conversion
            val tparams: List[t.TypeDef] = Nil
            val vparamss = List(convertTrees(vparams) map { _.asInstanceOf[t.ValDef] })
            val tTpt = t.TypeTree(convertType(rhs.tpe))
            val tRhs = convertTree(rhs)
            val defTp = convertSymbol(ld.symbol).termRef
            val dd = t.DefDef(convertToTermName(ld.name), tparams, vparamss, tTpt, tRhs) withType defTp
            val tIdent = t.Ident(dd.name) withType defTp
            val ap = t.Apply(tIdent, List())
            t.Block(List(dd), ap)
          } else {
            //LabelDef processing in pattern matching
            println(s"no implementation for LabelDef: ${g.showRaw(ld)}"); ???
          }
        case tr => println(s"no implementation for: ${g.showRaw(tr)}"); ???
      }
      resTree withPos tree.pos
      resTree
    }
  }

  def convertSelectors(iss: List[g.ImportSelector]): List[t.Tree] = iss map convertSelector

  def convertSelector(is: g.ImportSelector): t.Tree = {
    val n = is.name
    val r = is.rename
    if (n != r && r != null)
      t.Pair(t.Ident(n), t.Ident(r))
    else
      t.Ident(n)
  }

  def convertConstant(constant: g.Constant): tc.Constant = {
    constant.tag match {
      case g.ClazzTag => 
        val gConstTpVal = constant.typeValue
        val constTpeVal = convertType(gConstTpVal)
        tc.Constant(constTpeVal) withGConstType(constant.tpe)
      case g.EnumTag =>
        val gSymVal = constant.symbolValue
        val constSym = convertSymbol(gSymVal)
        tc.Constant(constSym) withGConstType(constant.tpe)
      case _ =>
        tc.Constant(constant.value)
    }
  }
    
}