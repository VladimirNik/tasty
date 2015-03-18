package scala.tasty.internal.scalac.pickler
package core

import PickleFormat._
import core._
import collection.mutable
import TastyBuffer._
import scala.tools.nsc.Global

trait TreePicklers extends NameBuffers
  with TastyNames
  with TastyPrinters
  with TastyReaders
  with TastyPicklers
  with TastyUnpicklers
  with TreeBuffers
  with PositionUnpicklers
  with PositionPicklers {
  val global: Global

  import global._
  class TreePickler(pickler: TastyPickler) {
    val buf = new TreeBuffer
    pickler.newSection("ASTs", buf)
    import buf._
    import pickler.nameBuffer.{ nameIndex, fullNameIndex }
//    import ast.tpd._

    private val symRefs = new mutable.HashMap[Symbol, Addr]
    private val forwardSymRefs = new mutable.HashMap[Symbol, List[Addr]]
    private val pickledTypes = new java.util.IdentityHashMap[Type, Any] // Value type is really Addr, but that's not compatible with null

    private def withLength(op: => Unit) = {
      val lengthAddr = reserveRef(relative = true)
      op
      fillRef(lengthAddr, currentAddr, relative = true)
    }

    private var makeSymbolicRefsTo: Symbol = NoSymbol

    /**
     * All references to members of class `sym` are pickled
     *  as symbolic references. Used to pickle the self info of a class.
     *  Without this precaution we get an infinite cycle when unpickling pos/extmethods.scala
     *  The problem arises when a self type of a trait is a type parameter of the same trait.
     */
    private def withSymbolicRefsTo[T](sym: Symbol)(op: => T): T = {
      val saved = makeSymbolicRefsTo
      makeSymbolicRefsTo = sym
      try op
      finally makeSymbolicRefsTo = saved
    }

    def preRegister(tree: Tree)/*(implicit ctx: Context)*/: Unit = tree match {
      case tree: MemberDef =>
        if (!symRefs.contains(tree.symbol)) symRefs(tree.symbol) = NoAddr
      case _ =>
    }

    def registerDef(sym: Symbol): Unit = {
      symRefs(sym) = currentAddr
      forwardSymRefs.get(sym) match {
        case Some(refs) =>
          refs.foreach(fillRef(_, currentAddr, relative = false))
          forwardSymRefs -= sym
        case None =>
      }
    }

    private def pickleName(name: Name) = writeNat(nameIndex(name).index)
    private def pickleName(name: TastyName) = writeNat(nameIndex(name).index)
    private def pickleNameAndSig(name: Name, sig: Signature) = {
      val Signature(params, result) = sig
      pickleName(TastyName.Signed(nameIndex(name), params.map(fullNameIndex), fullNameIndex(result)))
    }

    private def pickleSymRef(sym: Symbol)/*(implicit ctx: Context)*/ = symRefs.get(sym) match {
      case Some(label) =>
        if (label != NoAddr) writeRef(label) else pickleForwardSymRef(sym)
      case None =>
        //TODO - add log - originally was: ctx.log(i"...")
        println(s"pickling reference to as yet undefined $sym in ${sym.owner}", sym.pos)
        pickleForwardSymRef(sym)
    }

    private def pickleForwardSymRef(sym: Symbol)/*(implicit ctx: Context)*/ = {
      val ref = reserveRef(relative = false)
      assert(!sym.hasPackageFlag, sym)
      //TODO - check equality sym.hasPackageFlag - sym.is ...
      //assert(!sym.is(Flags.Package), sym)
      forwardSymRefs(sym) = ref :: forwardSymRefs.getOrElse(sym, Nil)
    }

    def pickle(trees: List[Tree])/*(implicit ctx: Context)*/ = {

      def qualifiedName(sym: Symbol): TastyName =
        if (sym.isRoot || sym.owner.isRoot) TastyName.Simple(sym.name.toTermName)
        else TastyName.Qualified(nameIndex(qualifiedName(sym.owner)), nameIndex(sym.name))

      def pickleConstant(c: Constant): Unit = c.tag match {
        case UnitTag =>
          writeByte(UNITconst)
        case BooleanTag =>
          writeByte(if (c.booleanValue) TRUEconst else FALSEconst)
        case ByteTag =>
          writeByte(BYTEconst)
          writeInt(c.byteValue)
        case ShortTag =>
          writeByte(SHORTconst)
          writeInt(c.shortValue)
        case CharTag =>
          writeByte(CHARconst)
          writeNat(c.charValue)
        case IntTag =>
          writeByte(INTconst)
          writeInt(c.intValue)
        case LongTag =>
          writeByte(LONGconst)
          writeLongInt(c.longValue)
        case FloatTag =>
          writeByte(FLOATconst)
          writeInt(java.lang.Float.floatToRawIntBits(c.floatValue))
        case DoubleTag =>
          writeByte(DOUBLEconst)
          writeLongInt(java.lang.Double.doubleToRawLongBits(c.doubleValue))
        case StringTag =>
          writeByte(STRINGconst)
          writeNat(nameIndex(c.stringValue).index)
        case NullTag =>
          writeByte(NULLconst)
        case ClazzTag =>
          writeByte(CLASSconst)
          pickleType(c.typeValue)
        case EnumTag =>
          writeByte(ENUMconst)
          //TODO - implement
          //probably - c.symbolValue.thisType
//          pickleType(c.symbolValue.termRef)
      }

      def pickleType(tpe0: Type, richTypes: Boolean = false): Unit = try {
        //TODO - see stripTypeVar - probably not required
//        val tpe = tpe0.stripTypeVar
        //TODO remove val tpe = tpe0
        val tpe = tpe0
        val prev = pickledTypes.get(tpe)
        if (prev == null) {
          pickledTypes.put(tpe, currentAddr)
          pickleNewType(tpe, richTypes)
        } else {
          writeByte(SHARED)
          writeRef(prev.asInstanceOf[Addr])
        }
      } catch {
        case ex: AssertionError =>
          println(s"error when pickling type $tpe0")
          throw ex
      }

      def pickleNewType(tpe: Type, richTypes: Boolean): Unit = try {
        tpe match {
          case ConstantType(value) =>
            pickleConstant(value)
//          case tpe: TypeRef if tpe.info.isAlias && tpe.symbol.is(Flags.AliasPreferred) =>
//            pickleType(tpe.info.bounds.hi)
          case tpe @ TypeRef(pre, sym, args) /*WithFixedSym*/ =>
            println(s"===> WithFixedSym")
//            val sym = tpe.symbol
            println(s"   sym: ${showRaw(sym)}")
            if (sym.hasPackageFlag /*sym.is(Flags.Package)*/) {
              println(s"   if - sym.is(Flags.Package)")
              writeByte(if (sym.isType /*TODO - fix it tpe.isType*/) TYPEREFpkg else TERMREFpkg)
              println(s"   tpe.isType: ${sym.isType}")
              println(s"   qualifiedName(sym): ${qualifiedName(sym)}")
              pickleName(qualifiedName(sym))
            }
//            } else if (tpe.prefix == NoPrefix) {
//              def pickleRef() = {
//                writeByte(if (tpe.isType) TYPEREFdirect else TERMREFdirect)
//                pickleSymRef(sym)
//              }
//              if (sym is Flags.BindDefinedType) {
//                registerDef(sym)
//                writeByte(BIND)
//                withLength {
//                  pickleName(sym.name)
//                  pickleType(sym.info)
//                  pickleRef()
//                }
//              } else pickleRef()
//            } else {
//              writeByte(if (tpe.isType) TYPEREFsymbol else TERMREFsymbol)
//              pickleSymRef(sym); pickleType(tpe.prefix)
//            }
          case tpe @ SingleType(pre, sym) /*TermRefWithSignature*/ =>
            println(s"===> TermRefWithSignature: ${showRaw(tpe)}")
            writeByte(TERMREF)
            println(s"   tpe.name: ${/*tpe*/ sym.name}")
            val sig = Signature(tpe)
            println(s"   tpe.signature: ${sig}")
            println(s"   tpe.signature.resSig: ${sig.resSig}")
            println(s"   tpe.prefix: ${showRaw(tpe.prefix)}")
            pickleNameAndSig(/*tpe*/ sym.name, /*tpe.signature*/ sig); pickleType(tpe.prefix)
          case tpe: NamedType =>
//            if (tpe.name == tpnme.Apply && tpe.prefix.argInfos.nonEmpty && tpe.prefix.isInstantiatedLambda)
//              // instantiated lambdas are pickled as APPLIEDTYPE; #Apply will 
//              // be reconstituted when unpickling.
//              pickleType(tpe.prefix)
//            else tpe.prefix match {
//              case prefix: ThisType if prefix.cls == makeSymbolicRefsTo =>
//                pickleType(NamedType.withFixedSym(tpe.prefix, tpe.symbol))
//              case _ =>
//                writeByte(if (tpe.isType) TYPEREF else TERMREF)
//                pickleName(tpe.name); pickleType(tpe.prefix)
//            }
          case tpe: ThisType =>
            println(s"===> ThisType: ${showRaw(tpe)}")
            writeByte(THIS)
            println(s"   tpe.tref: ${showRaw(tpe.typeOfThis)}")
            pickleType(tpe.typeOfThis)
          case tpe: SuperType =>
            writeByte(SUPERtype)
            withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe) }
//          case tpe: SkolemType =>
//            writeByte(SKOLEMtype)
//            writeRef(pickledTypes.get(tpe.binder).asInstanceOf[Addr])
          case tpe: RefinedType =>
//            val args = tpe.argInfos(interpolate = false)
//            if (args.isEmpty) {
//              writeByte(REFINEDtype)
//              withLength {
//                pickleType(tpe.parent)
//                pickleName(tpe.refinedName)
//                pickleType(tpe.refinedInfo, richTypes = true)
//              }
//            } else {
//              writeByte(APPLIEDtype)
//              withLength { pickleType(tpe.withoutArgs(args)); args.foreach(pickleType(_)) }
//            }
//          case tpe: TypeAlias =>
//            writeByte(TYPEALIAS)
//            withLength {
//              pickleType(tpe.alias, richTypes)
//              tpe.variance match {
//                case 1  => writeByte(COVARIANT)
//                case -1 => writeByte(CONTRAVARIANT)
//                case 0  =>
//              }
//            }
          case tpe: TypeBounds =>
            writeByte(TYPEBOUNDS)
            withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
          case tpe: AnnotatedType =>
            writeByte(ANNOTATED)
//            withLength { pickleTree(tpe.annot.tree); pickleType(tpe.tpe, richTypes) }
//          case tpe: AndOrType =>
//            writeByte(if (tpe.isAnd) ANDtype else ORtype)
//            withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
//          case tpe: ExprType =>
//            writeByte(BYNAMEtype)
//            pickleType(tpe.underlying)
          case tpe: MethodType if richTypes =>
            writeByte(METHODtype)
//            pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramTypes)
          case tpe: PolyType if richTypes =>
            writeByte(POLYtype)
//            pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramBounds)
//          case tpe: PolyParam =>
//            if (!pickleParamType(tpe))
//              // TODO figure out why this case arises in e.g. pickling AbstractFileReader.
//              ctx.typerState.constraint.entry(tpe) match {
//                case TypeBounds(lo, hi) if lo eq hi => pickleNewType(lo, richTypes)
//                case _                              => assert(false, s"orphan poly parameter: $tpe")
//              }
//          case tpe: MethodParam =>
//            assert(pickleParamType(tpe), s"orphan method parameter: $tpe")
//          case tpe: LazyRef =>
//            pickleType(tpe.ref)
          case NoType =>
            writeByte(NOTYPE)
          //      case NoPrefix =>    // not sure we need this!
          //        writeByte(NOPREFIX)
          case _ =>
        }
      } catch {
        case ex: AssertionError =>
          println(s"error while pickling type $tpe")
          throw ex
      }

      def pickleMethodic(result: Type, names: List[Name], types: List[Type]) =
        withLength {
          pickleType(result, richTypes = true)
          (names, types).zipped.foreach { (name, tpe) =>
            pickleName(name); pickleType(tpe)
          }
        }

//      def pickleParamType(tpe: ParamType): Boolean = {
//        val binder = pickledTypes.get(tpe.binder)
//        val pickled = binder != null
//        if (pickled) {
//          writeByte(PARAMtype)
//          withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
//        }
//        pickled
//      }

      def pickleTpt(tpt: Tree): Unit = pickleType(tpt.tpe) // TODO correlate with original when generating positions

      def pickleTreeUnlessEmpty(tree: Tree): Unit =
        if (!tree.isEmpty) pickleTree(tree)

      def pickleTree(tree: Tree): Unit = try {
        pickledTrees.put(tree, currentAddr)
        tree match {
          case Ident(name) =>
            tree.tpe match {
                //TODO - implement (what is TermRef?)
//              case tp: TermRef => pickleType(tp)
              case _ =>
                writeByte(IDENT)
                pickleName(name)
                pickleType(tree.tpe)
            }
          case This(_) =>
            pickleType(tree.tpe)
          //TODO - fix in select
          case Select(qual, name) =>
            writeByte(SELECT)
            val realName = tree.tpe match {
              case tp: NamedType if isShadowedName(tp.name)/*tp.name.isShadowedName*/ => tp.name
              case _                                       => name
            }
            val sig = Signature(tree.tpe) //tree.tpe.signature
            if (sig.notAMethod /*Signature.NotAMethod*/) pickleName(realName)
            else pickleNameAndSig(realName, sig)
            pickleTree(qual)
          case Apply(fun, args) =>
            writeByte(APPLY)
            withLength {
              pickleTree(fun)
              args.foreach(pickleTree)
            }
          case TypeApply(fun, args) =>
            writeByte(TYPEAPPLY)
            withLength {
              pickleTree(fun)
              args.foreach(pickleTpt)
            }
          case Literal(const1) =>
            pickleConstant {
              tree.tpe match {
                case ConstantType(const2) => const2
                case _                    => const1
              }
            }
          case Super(qual, mix) =>
            writeByte(SUPER)
            withLength {
              pickleTree(qual);
              if (!mix.isEmpty) {
                val SuperType(_, mixinType) = tree.tpe
                pickleType(mixinType)
              }
            }
          case New(tpt) =>
            writeByte(NEW)
            pickleTpt(tpt)
//          case Pair(left, right) =>
//            writeByte(PAIR)
//            withLength { pickleTree(left); pickleTree(right) }
          case Typed(expr, tpt) =>
            writeByte(TYPED)
            withLength { pickleTree(expr); pickleTpt(tpt) }
//          case NamedArg(name, arg) =>
//            writeByte(NAMEDARG)
//            withLength { pickleName(name); pickleTree(arg) }
          case Assign(lhs, rhs) =>
            writeByte(ASSIGN)
            withLength { pickleTree(lhs); pickleTree(rhs) }
          case Block(stats, expr) =>
            writeByte(BLOCK)
            stats.foreach(preRegister)
            withLength { pickleTree(expr); stats.foreach(pickleTree) }
          case If(cond, thenp, elsep) =>
            writeByte(IF)
            withLength { pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
//          case Closure(env, meth, tpt) =>
//            writeByte(CLOSURE)
//            withLength { pickleTree(meth); pickleTpt(tpt); env.foreach(pickleTree) }
          case Match(selector, cases) =>
            writeByte(MATCH)
            withLength { pickleTree(selector); cases.foreach(pickleTree) }
          case CaseDef(pat, guard, rhs) =>
            writeByte(CASEDEF)
            withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
//          case Return(expr, from) =>
//            writeByte(RETURN)
//            withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
          case Try(block, cases, finalizer) =>
            writeByte(TRY)
            withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
//          case SeqLiteral(elems) =>
//            writeByte(REPEATED)
//            withLength { elems.foreach(pickleTree) }
          case TypeTree()/*(original)*/ =>
            pickleTpt(tree)
          case Bind(name, body) =>
            registerDef(tree.symbol)
            writeByte(BIND)
            withLength { pickleName(name); pickleType(tree.symbol.info); pickleTree(body) }
          case Alternative(alts) =>
            writeByte(ALTERNATIVE)
            withLength { alts.foreach(pickleTree) }
//          case UnApply(fun, implicits, patterns) =>
//            writeByte(UNAPPLY)
//            withLength {
//              pickleTree(fun)
//              for (implicitArg <- implicits) {
//                writeByte(IMPLICITarg)
//                pickleTree(implicitArg)
//              }
//              pickleType(tree.tpe)
//              patterns.foreach(pickleTree)
//            }
          case tree: ValDef =>
            pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
          case tree: DefDef =>
            def pickleAllParams = {
              pickleParams(tree.tparams)
              for (vparams <- tree.vparamss) {
                writeByte(PARAMS)
                withLength { pickleParams(vparams) }
              }
            }
            pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleAllParams)
          case tree: TypeDef =>
            pickleDef(TYPEDEF, tree.symbol, tree.rhs)
          case tree: ClassDef =>
            println(s"***> TypeDef: $tree")
            pickleDef(TYPEDEF, tree.symbol, tree.impl)
          case tree: Template =>
            println(s"***> TypeDef: $tree")
            //TODO - see treeInfo
            val primaryCtr = treeInfo.firstConstructor(tree.body)
            val firstCtrArgs = treeInfo.firstConstructorArgs(tree.body)
            println(s"primaryCtr: ${showRaw(primaryCtr)}")
            println(s"firstCtrArgs: $firstCtrArgs")
            registerDef(tree.symbol)
            writeByte(TEMPLATE)
            val (params, rest) = tree.body partition {
              case stat: TypeDef => stat.symbol.isParameter// TODO - check - originally is Flags.Param
              case stat: ValOrDefDef =>
                stat.symbol.isParamAccessor /* TODO - check equality (Flags.ParamAccessor)*/ && !stat.symbol.isSetter
              case _ => false
            }
            withLength {
              pickleParams(params)
              tree.parents.foreach(pickleTree)
              //TODO - implement
              val cinfo @ ClassInfoType(_, _, _) = tree.symbol.owner.info
//              val cinfo @ ClassInfo(_, _, _, _, selfInfo) = tree.symbol.owner.info
              if (/*TODO - selfInfo in Scala? (selfInfo ne NoType) ||*/ !tree.self.isEmpty) {
                writeByte(SELFDEF)
                pickleName(tree.self.name)
                withSymbolicRefsTo(tree.symbol.owner) {
                //TODO - check what's used in the example
//                  pickleType {
//                    cinfo.selfInfo match {
//                      case sym: Symbol => sym.info
//                      case tp: Type    => tp
//                    }
//                  }
                }
              }
              pickleStats(rest)
              //TODO - check - probably constructor is in the rest
//              pickleStats(tree.constr :: rest)
            }
          case Import(expr, selectors) =>
            writeByte(IMPORT)
            withLength {
              pickleTree(expr)
//              selectors foreach {
//                case Pair(Ident(from), Ident(to)) =>
//                  writeByte(RENAMED)
//                  withLength { pickleName(from); pickleName(to) }
//                case Ident(name) =>
//                  writeByte(IMPORTED)
//                  pickleName(name)
//              }
            }
          case PackageDef(pid, stats) =>
            println(s"***> PackageDef: $tree")
            writeByte(PACKAGE)
            withLength { pickleType(pid.tpe); pickleStats(stats) }
        }
      } catch {
        case ex: AssertionError =>
          println(s"error when pickling tree $tree")
          throw ex
      }

      def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ()) = {
        assert(symRefs(sym) == NoAddr)
        registerDef(sym)
        writeByte(tag)
        withLength {
          println(s"   pickleName: ${sym.name}")
          pickleName(sym.name)
          pickleParams
          println(s"   tpt: ${showRaw(tpt)}")
          tpt match {
            case tpt: TypeTree => 
              println("   case tpt: TypeTree")
              pickleTpt(tpt)
            case _             => 
              println("   case _")
              pickleTree(tpt)
          }
          pickleTreeUnlessEmpty(rhs)
          //TODO - check correctness of pickled modifiers
          pickleModifiers(sym)
        }
      }

      def pickleParam(tree: Tree): Unit = tree match {
        case tree: ValDef  => pickleDef(PARAM, tree.symbol, tree.tpt)
        case tree: DefDef  => pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
        case tree: TypeDef => pickleDef(TYPEPARAM, tree.symbol, tree.rhs)
      }

      def pickleParams(trees: List[Tree]): Unit = {
        trees.foreach(preRegister)
        trees.foreach(pickleParam)
      }

      def pickleStats(stats: List[Tree]) = {
        stats.foreach(preRegister)
        stats.foreach(stat => if (!stat.isEmpty) pickleTree(stat))
      }

      def pickleModifiers(sym: Symbol): Unit = {
//        import Flags._
        val flags = sym.flags
        val privateWithin = sym.privateWithin
        if (privateWithin.exists) {
          writeByte(if (sym.isProtected /*flags is Protected*/) PROTECTEDqualified else PRIVATEqualified)
          //TODO - fix it
//          pickleType(privateWithin.typeRef)
        }
        if (sym.isPrivate /*flags is Private*/) writeByte(PRIVATE)
        if (sym.isProtected /*flags is Protected*/) if (!privateWithin.exists) writeByte(PROTECTED)
        if (sym.isFinal /*flags is Final*/) writeByte(FINAL)
        if (sym.isCase /*flags is Case*/) writeByte(CASE)
        if (sym.isOverride /*flags is Override*/) writeByte(OVERRIDE)
//        if (flags is Inline) writeByte(INLINE)
//        if (flags is JavaStatic) writeByte(STATIC)
        if (sym.isModule /*flags is Module*/) writeByte(MODULE)
        if (sym.isLocalToBlock /*flags is Local*/) writeByte(LOCAL)
        if (sym.isSynthetic /*flags is Synthetic*/) writeByte(SYNTHETIC)
        if (sym.isArtifact /*flags is Artifact*/) writeByte(ARTIFACT)
//        if (flags is Scala2x) writeByte(SCALA2X)
//        if (flags is InSuperCall) writeByte(INSUPERCALL)
        if (sym.isTerm) {
          if (sym.isImplicit /*flags is Implicit*/) writeByte(IMPLICIT)
          if (sym.isLazy /*flags is Lazy*/) writeByte(LAZY)
          if (sym.isAbstractOverride /*flags is AbsOverride*/) writeByte(ABSOVERRIDE)
          if (sym.isMutable /*flags is Mutable*/) writeByte(MUTABLE)
          if (sym.isAccessor /*flags is Accessor*/) writeByte(FIELDaccessor)
          if (sym.isCaseAccessor /*flags is CaseAccessor*/) writeByte(CASEaccessor)
//          if (flags is DefaultParameterized) writeByte(DEFAULTparameterized)
//          if (flags is DefaultInit) writeByte(DEFAULTinit)
        } else {
          if (sym.isSealed /*flags is Sealed*/) writeByte(SEALED)
          if (sym.isAbstract /*flags is Abstract*/) writeByte(ABSTRACT)
          if (sym.isTrait /*flags is Trait*/) writeByte(TRAIT)
          if (sym.isCovariant /*flags is Covariant*/) writeByte(COVARIANT)
          if (sym.isContravariant /*flags is Contravariant*/) writeByte(CONTRAVARIANT)
        }
        sym.annotations.foreach(pickleAnnotation)
      }

      def pickleAnnotation(ann: Annotation) = {
        writeByte(ANNOTATION)
//        withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
      }

      trees.foreach(tree => if (!tree.isEmpty) pickleTree(tree))
      assert(forwardSymRefs.isEmpty, s"unresolved symbols: ${forwardSymRefs.keySet.toList}%, %")
      compactify()
    }
  }
}
