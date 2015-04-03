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
      log("==> wl")
      val lengthAddr = reserveRef(relative = true)
      op
      fillRef(lengthAddr, currentAddr, relative = true)
      log("<== wl")
    }

    def addrOfSym(sym: Symbol): Option[Addr] = {
      symRefs.get(sym)
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

    private def pickleName(name: Name) = {
      log(s"pickleName: ${name.toString()}")
      writeNat(nameIndex(name).index)
    }
    private def pickleName(name: TastyName) = {
      log(s"pickleName(*): ${name.toString()}")
      writeNat(nameIndex(name).index)
    }
    private def pickleNameAndSig(name: Name, sig: Signature) = {
      val Signature(params, result) = sig
      log(s"pickleNameAndSig, name: ${name.toString()}")
      log(s"                  params: ${params.map(_.toString())}")
      log(s"                  result: ${result.toString()}")
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

    var debugCond = true
    var logCond = true
  
    def setDebugCond(tr: Tree) = true /*tr match {
      case ValDef(name, _, _) if name.toString() == "testCodeInNextCases" => debugCond = true
      case ValDef(name, _, _) if name.toString() == "body" => debugCond = false
      case _ =>
    }*/
  
    def debug(str: String) = if (debugCond) println(str)
    
    def log(str: String) = if (logCond) println(">>>>>  " + str)
    
    def pickle(trees: List[Tree])/*(implicit ctx: Context)*/ = {
      println("=== PICKLING STARTED ===")
      
      def qualifiedName(sym: Symbol): TastyName =
        if (sym.isRoot || sym.owner.isRoot) TastyName.Simple(sym.name.toTermName)
        else TastyName.Qualified(nameIndex(qualifiedName(sym.owner)), nameIndex(sym.name))

      def pickleConstant(c: Constant): Unit = {
        log(s"pickleConstant: ${c}")
        c.tag match {
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
            debug(s"                  pickleType: ${c.typeValue}")
            pickleType(c.typeValue)
          case EnumTag =>
            writeByte(ENUMconst)
          //TODO - implement
          //probably - c.symbolValue.thisType
          //debug(s"                  pickleType: ${c.symbolValue.termRef}")
          //          pickleType(c.symbolValue.termRef)
        }
      }

      def pickleType(tpe0: Type, richTypes: Boolean = false): Unit = try {
        //TODO - see stripTypeVar - probably not required
//        val tpe = tpe0.stripTypeVar
        debug(s"pickleType: ${tpe0}")
        debug(s"            richTypes: ${richTypes}")
        //TODO remove val tpe = tpe0
        val tpe = tpe0
        val prev = pickledTypes.get(tpe)
        if (prev == null) {
          pickledTypes.put(tpe, currentAddr)
          pickleNewType(tpe, richTypes)
        } else {
          log(s"        SHARED")
          writeByte(SHARED)
          writeRef(prev.asInstanceOf[Addr])
        }
      } catch {
        case ex: AssertionError =>
          println(s"error when pickling type $tpe0")
          throw ex
      }

      def pickleNewType(tpe: Type, richTypes: Boolean): Unit = try {
        if (tpe.toString().contains("RefinedType(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,core)),Symbols$)),Symbol), ThisName, TypeAlias(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,core)),Names),TermName)")) {
          debugCond = true
        }
        debug("")
        debug(s"TYPE pickleNewType: ${tpe}")
        debug(s"    richTypes: $richTypes")
        tpe match {
          case ConstantType(value) =>
            debug(s"     *** ConstantType ***")
            pickleConstant(value)
            debug(s"     --- ConstantType ***")
//          case tpe: TypeRef if tpe.info.isAlias && tpe.symbol.is(Flags.AliasPreferred) =>
//            debug(s"     *** TypeRef if tpe.info.isAlias && tpe.symbol.is(Flags.AliasPreferred) ***")
//            pickleType(tpe.info.bounds.hi)
//            debug(s"     --- TypeRef if ... ***")
          case tpe @ TypeRef(pre, sym, args) /*WithFixedSym*/ =>
            debug(s"     *** WithFixedSym (scala - TypeRef(pre, sym, args)) ***")
//            val sym = tpe.symbol
            debug(s"   sym: ${showRaw(sym)}")
            if (sym.hasPackageFlag /*sym.is(Flags.Package)*/) {
              debug(s"     sym.is(Flags.Package): ${sym.hasPackageFlag}")
              writeByte(if (sym.isType /*TODO - fix it tpe.isType*/) {log(s"     byte: TYPEREFpkg"); TYPEREFpkg} else {log(s"     byte: TERMREFpkg"); TERMREFpkg})
              debug(s"       tpe.isType: ${sym.isType}")
              debug(s"       qualifiedName(sym): ${qualifiedName(sym)}")
              pickleName(qualifiedName(sym))
            }
//            } else if (tpe.prefix == NoPrefix) {
//              debug(s"     if tpe.prefix == NoPrefix")
//              def pickleRef() = {
//                writeByte(if (tpe.isType) {log(s"     byte: TYPEREFdirect"); TYPEREFdirect} else {log(s"     byte: TERMREFdirect"); TERMREFdirect})
//                pickleSymRef(sym)
//              }
//              if (sym is Flags.BindDefinedType) {
//                registerDef(sym)
//                log(s"     byte: BIND")
//                writeByte(BIND)
//                withLength {
//                  pickleName(sym.name)
//                  pickleType(sym.info)
//                  pickleRef()
//                }
//              } else pickleRef()
//            } else {
//              debug(s"     else")
//              writeByte(if (tpe.isType) {log(s"     byte: TYPEREFsymbol"); TYPEREFsymbol} else {log(s"     byte: TERMREFsymbol"); TERMREFsymbol})
//              pickleSymRef(sym); pickleType(tpe.prefix)
//            }
            debug(s"     --- WithFixedSym ***")
          case tpe @ SingleType(pre, sym) /*TermRefWithSignature*/ =>
            debug(s"     *** TermRefWithSignature ***")
            debug(s"===> TermRefWith...(scala - SingleType): ${showRaw(tpe)}")
            log(s"     byte: TERMREF");
            writeByte(TERMREF)
            debug(s"   tpe.name: ${/*tpe*/ sym.name}")
            val sig = Signature(tpe)
            debug(s"   tpe.signature: ${sig}")
            debug(s"   tpe.signature.resSig: ${sig.resSig}")
            debug(s"   tpe.prefix: ${showRaw(tpe.prefix)}")
            pickleNameAndSig(/*tpe*/ sym.name, /*tpe.signature*/ sig); pickleType(tpe.prefix)
            debug(s"     --- TermRefWithSignature ***")
          case tpe: NamedType =>
            debug(s"     *** NamedType ***")
//            if (tpe.name == tpnme.Apply && tpe.prefix.argInfos.nonEmpty && tpe.prefix.isInstantiatedLambda) {
//              // instantiated lambdas are pickled as APPLIEDTYPE; #Apply will 
//              // be reconstituted when unpickling.
//              debug(s"     if (tpe.name == tpnme.Apply && tpe.prefix.argInfos.nonEmpty && tpe.prefix.isInstantiatedLambda)")
//              pickleType(tpe.prefix)
//            } else {
//              debug(s"     else")
//              tpe.prefix match {
//              case prefix: ThisType if prefix.cls == makeSymbolicRefsTo =>
//                debug(s"     case prefix: ThisType if prefix.cls == makeSymbolicRefsTo =>")
//                pickleType(NamedType.withFixedSym(tpe.prefix, tpe.symbol))
//              case _ =>
//                writeByte(if (tpe.isType) {log(s"     byte: TYPEREF"); TYPEREF} else {log(s"     byte: TERMREF"); TERMREF})
//                pickleName(tpe.name); pickleType(tpe.prefix)
//            }}
            debug(s"     --- NamedType ***")
          case tpe: ThisType =>
            debug(s"     *** ThisType ***")
            log(s"     byte: THIS");
            writeByte(THIS)
            debug(s"   tpe.tref: ${showRaw(tpe.typeOfThis)}")
            pickleType(tpe.typeOfThis)
            debug(s"     --- ThisType ***")
          case tpe: SuperType =>
            debug(s"     *** SuperType ***")
            log(s"     byte: SUPERtype");
            writeByte(SUPERtype)
            withLength { pickleType(tpe.thistpe); pickleType(tpe.supertpe) }
            debug(s"     --- SuperType ***")
//          case tpe: SkolemType =>
//            debug(s"     *** SkolemType ***")
//            log(s"     byte: SKOLEMtype");
//            writeByte(SKOLEMtype)
//            writeRef(pickledTypes.get(tpe.binder).asInstanceOf[Addr])
//            debug(s"     --- SkolemType ***")
          case tpe: RefinedType =>
            debug(s"     *** RefinedType ***")
//            val args = tpe.argInfos(interpolate = false)
//            debug(s"     args.size: ${args.size}"
//            if (args.isEmpty) {
//              log(s"     byte: REFINEDtype");
//              writeByte(REFINEDtype)
//              withLength {
//                pickleType(tpe.parent)
//                pickleName(tpe.refinedName)
//                pickleType(tpe.refinedInfo, richTypes = true)
//              }
//            } else {
//              log(s"     byte: APPLIEDtype");
//              writeByte(APPLIEDtype)
//              withLength { pickleType(tpe.withoutArgs(args)); args.foreach(pickleType(_)) }
//            }
            debug(s"     --- RefinedType ***")
//          case tpe: TypeAlias =>
//            debug(s"     *** TypeAlias ***")
//            log(s"     byte: TYPEALIAS");
//            writeByte(TYPEALIAS)
//            withLength {
//              pickleType(tpe.alias, richTypes)
//              tpe.variance match {
//                case 1  => 
//                  log(s"     byte: COVARIANT");
//                  writeByte(COVARIANT)
//                case -1 => 
//                  log(s"     byte: CONTRAVARIANT");
//                  writeByte(CONTRAVARIANT)
//                case 0  =>
//              }
//            }
//            debug(s"     --- TypeAlias ***")
          case tpe: TypeBounds =>
            debug(s"     *** TypeBounds ***")
            log(s"     byte: TYPEBOUNDS");
            writeByte(TYPEBOUNDS)
            withLength { pickleType(tpe.lo, richTypes); pickleType(tpe.hi, richTypes) }
            debug(s"     --- TypeBounds ***")
          case tpe: AnnotatedType =>
            debug(s"     *** AnnotatedType ***")
            log(s"     byte: ANNOTATED");
            writeByte(ANNOTATED)
//            withLength { pickleTree(tpe.annot.tree); pickleType(tpe.tpe, richTypes) }
            debug(s"     --- AnnotatedType ***")
//          case tpe: AndOrType =>
//            debug(s"     *** AndOrType ***")
//            writeByte(if (tpe.isAnd) {log(s"     byte: ANDtype"); ANDtype} else {log(s"     byte: ORtype"); ORtype})
//            withLength { pickleType(tpe.tp1, richTypes); pickleType(tpe.tp2, richTypes) }
//            debug(s"     --- AndOrType ***")
//          case tpe: ExprType =>
//            debug(s"     *** ExprType ***")
//            log(s"     byte: BYNAMEtype");
//            writeByte(BYNAMEtype)
//            pickleType(tpe.underlying)
//            debug(s"     --- ExprType ***")
          case tpe: MethodType if richTypes =>
            debug(s"     *** MethodType if richTypes ***")
            log(s"     byte: METHODtype");
            writeByte(METHODtype)
//            pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramTypes)
            debug(s"     --- MethodType if richTypes ***")
          case tpe: PolyType if richTypes =>
            debug(s"     *** PolyType if richTypes ***")
            log(s"     byte: POLYtype");
            writeByte(POLYtype)
//            pickleMethodic(tpe.resultType, tpe.paramNames, tpe.paramBounds)
            debug(s"     --- PolyType if richTypes ***")
//          case tpe: PolyParam =>
//            debug(s"     *** PolyParam ***")
//            if (!pickleParamType(tpe)) {
//              debug(s"     !pickleParamType(tpe)");
//              // TODO figure out why this case arises in e.g. pickling AbstractFileReader.
//              ctx.typerState.constraint.entry(tpe) match {
//                case TypeBounds(lo, hi) if lo eq hi => 
//                  debug(s"     case TypeBounds...");
//                  pickleNewType(lo, richTypes)
//                case _                              => 
//                  debug(s"     case _ =>");
//                  assert(false, s"orphan poly parameter: $tpe")
//              }}
//            debug(s"     --- PolyParam ***")
//          case tpe: MethodParam =>
//            debug(s"     *** MethodParam ***")
//            assert(pickleParamType(tpe), s"orphan method parameter: $tpe")
//            debug(s"     --- MethodParam ***")
//          case tpe: LazyRef =>
//            debug(s"     *** LazyRef ***")
//            pickleType(tpe.ref)
//            debug(s"     --- LazyRef ***")
//          case NoType =>
//            writeByte(NOTYPE)
          //      case NoPrefix =>    // not sure we need this!
          //        writeByte(NOPREFIX)
          case _ =>
        }
        debug("")
      } catch {
        case ex: AssertionError =>
          println(s"error while pickling type $tpe")
          throw ex
      }

      def pickleMethodic(result: Type, names: List[Name], types: List[Type]) = {
        debug(s"debug - pickleMethodic");
        withLength {
          pickleType(result, richTypes = true)
          (names, types).zipped.foreach { (name, tpe) =>
            pickleName(name); pickleType(tpe)
          }
        }
      }

//      def pickleParamType(tpe: ParamType): Boolean = {
//        debug(s"debug - pickleParamType");
//        debug(s"        binder: ${tpe.binder}");
//        val binder = pickledTypes.get(tpe.binder)
//        val pickled = binder != null
//        if (pickled) {
//          log(s"     byte: PARAMtype");
//          writeByte(PARAMtype)
//          withLength { writeRef(binder.asInstanceOf[Addr]); writeNat(tpe.paramNum) }
//        }
//        pickled
//      }

      def pickleTpt(tpt: Tree): Unit = pickleType(tpt.tpe) // TODO correlate with original when generating positions

      def pickleTreeUnlessEmpty(tree: Tree): Unit =
        if (!tree.isEmpty) pickleTree(tree)

      def pickleTree(tree: Tree): Unit = try {
        //only for debug purposes (to see the code between val testCodeInNextCases ... and val body ...)
        setDebugCond(tree)
        debug("")
        debug(s"TREE pickleTree: ${tree}")
        
        pickledTrees.put(tree, currentAddr)
        tree match {
          case Ident(name) =>
            debug(s"     @@@ Ident($name) @@@")
            debug(s"     tree.tpe: ${tree.tpe}")
            tree.tpe match {
                //TODO - implement (what is TermRef?)
//              debug(s"     case tp: TermRef")
//              case tp: TermRef => pickleType(tp)
              case _ =>
                log(s"     byte: IDENT")
                writeByte(IDENT)
                pickleName(name)
                pickleType(tree.tpe)
            }
            debug(s"     === Ident($name) @@@")
          case This(_) =>
            debug(s"     @@@ This @@@")
            debug(s"     tree.tpe: ${tree.tpe}")
            pickleType(tree.tpe)
            debug(s"     === This @@@")
          //TODO - fix in select
          case Select(qual, name) =>
            debug(s"     @@@ Select(qual, $name) @@@")
            log(s"     byte: SELECT")
            writeByte(SELECT)
            val realName = tree.tpe match {
              case tp: NamedType if isShadowedName(tp.name)/*tp.name.isShadowedName*/ => tp.name
              case _                                       => name
            }
            val sig = Signature(tree.tpe) //tree.tpe.signature
            if (sig.notAMethod /*Signature.NotAMethod*/) pickleName(realName)
            else pickleNameAndSig(realName, sig)
            pickleTree(qual)
            debug(s"     === Select @@@")
          case Apply(fun, args) =>
            debug(s"     @@@ Apply(fun, args) @@@")
            debug(s"     fun: $fun")
            debug(s"     args.size: ${args.size}")
            log(s"     byte: APPLY")
            writeByte(APPLY)
            withLength {
              pickleTree(fun)
              args.foreach(pickleTree)
            }
            debug(s"     === Apply @@@")
          case TypeApply(fun, args) =>
            debug(s"     @@@ TypeApply(fun, args) @@@")
            debug(s"     fun: $fun")
            debug(s"     args.size: ${args.size}")
            log(s"     byte: TYPEAPPLY")
            writeByte(TYPEAPPLY)
            withLength {
              pickleTree(fun)
              args.foreach(pickleTpt)
            }
            debug(s"     === TypeApply @@@")
          case Literal(const1) =>
            debug(s"     @@@ Literal($const1) @@@")
            pickleConstant {
              tree.tpe match {
                case ConstantType(const2) => const2
                case _                    => const1
              }
            }
            debug(s"     === Literal @@@")
          case Super(qual, mix) =>
            debug(s"     @@@ Super(qual, mix) @@@")
            debug(s"     mix: $mix")
            log(s"     byte: SUPER")
            writeByte(SUPER)
            withLength {
              pickleTree(qual);
              if (!mix.isEmpty) {
                debug(s"     !mix.isEmpty")
                debug(s"     tree.tpe: ${tree.tpe}")
                val SuperType(_, mixinType) = tree.tpe
                debug(s"     mixinType: ${mixinType}")
                pickleType(mixinType)
              }
            }
            debug(s"     === Super @@@")
          case New(tpt) =>
            debug(s"     @@@ New($tpt) @@@")
            log(s"     byte: NEW")
            writeByte(NEW)
            pickleTpt(tpt)
            debug(s"     === New @@@")
//          case Pair(left, right) =>
//            debug(s"     @@@ Pair @@@")
//            log(s"     byte: PAIR")
//            writeByte(PAIR)
//            withLength { pickleTree(left); pickleTree(right) }
//            debug(s"     === Pair @@@")
          case Typed(expr, tpt) =>
            debug(s"     @@@ Typed(expr, $tpt) @@@")
            log(s"     byte: TYPED")
            writeByte(TYPED)
            withLength { pickleTree(expr); pickleTpt(tpt) }
            debug(s"     === Typed @@@")
//          case NamedArg(name, arg) =>
//            debug(s"     @@@ NamedArg($name, arg) @@@")
//            log(s"     byte: NAMEDARG")
//            writeByte(NAMEDARG)
//            withLength { pickleName(name); pickleTree(arg) }
//            debug(s"     === NamedArg @@@")
          case Assign(lhs, rhs) =>
            debug(s"     @@@ Assign(lhs, rhs) @@@")
            log(s"     byte: ASSIGN")
            writeByte(ASSIGN)
            withLength { pickleTree(lhs); pickleTree(rhs) }
            debug(s"     === Assign @@@")
          case Block(stats, expr) =>
            debug(s"     @@@ Block(stats, expr) @@@")
            log(s"     byte: BLOCK")
            writeByte(BLOCK)
            stats.foreach(preRegister)
            withLength { pickleTree(expr); stats.foreach(pickleTree) }
            debug(s"     === Block @@@")
          case If(cond, thenp, elsep) =>
            debug(s"     @@@ If(cond, thenp, elsep) @@@")
            log(s"     byte: IF")
            writeByte(IF)
            withLength { pickleTree(cond); pickleTree(thenp); pickleTree(elsep) }
            debug(s"     === If @@@")
//          case Closure(env, meth, tpt) =>
//            debug(s"     @@@ Closure(env, meth, tpt) @@@")
//            log(s"     byte: LAMBDA")
//            writeByte(LAMBDA)
//            withLength { pickleTree(meth); pickleTpt(tpt); env.foreach(pickleTree) }
          case Match(selector, cases) =>
            debug(s"     @@@ Match(selector, cases) @@@")
            debug(s"     selector: $selector")
            cases.foreach { x => debug(s"     case: $x") }
            log(s"     byte: MATCH")
            writeByte(MATCH)
            withLength { pickleTree(selector); cases.foreach(pickleTree) }
            debug(s"     === Match @@@")
          case CaseDef(pat, guard, rhs) =>
            debug(s"     @@@ CaseDef(pat, guard, rhs) @@@")
            log(s"     byte: CASEDEF")
            writeByte(CASEDEF)
            debug(s"     pat: $pat")
            debug(s"     guard: $guard")
            debug(s"     rhs: $rhs")
            withLength { pickleTree(pat); pickleTree(rhs); pickleTreeUnlessEmpty(guard) }
            debug(s"     === CaseDef @@@")
//          case Return(expr, from) =>
//            debug(s"     @@@ Return(expr, from) @@@")
//            log(s"     byte: RETURN")
//            writeByte(RETURN)
//            withLength { pickleSymRef(from.symbol); pickleTreeUnlessEmpty(expr) }
//            debug(s"     === Return @@@")
          case Try(block, cases, finalizer) =>
            debug(s"     @@@ Try @@@")
            log(s"     byte: TRY")
            writeByte(TRY)
            withLength { pickleTree(block); cases.foreach(pickleTree); pickleTreeUnlessEmpty(finalizer) }
            debug(s"     === Try @@@")
//          case SeqLiteral(elems) =>
//            debug(s"     @@@ SeqLiteral @@@")
//            log(s"     byte: REPEATED")
//            writeByte(REPEATED)
//            withLength { elems.foreach(pickleTree) }
//            debug(s"     === SeqLiteral @@@")
          case TypeTree()/*(original)*/ =>
            debug(s"     @@@ TypeTree @@@")
            pickleTpt(tree)
            debug(s"     === TypeTree @@@")
          case Bind(name, body) =>
            debug(s"     @@@ Bind($name, body) @@@")
            registerDef(tree.symbol)
            log(s"     byte: BIND")
            writeByte(BIND)
            debug(s"     tree.symbol: ${tree.symbol}")
            debug(s"     tree.symbol.info: ${tree.symbol.info}")
            debug(s"     body: ${body}")
            withLength { pickleName(name); pickleType(tree.symbol.info); pickleTree(body) }
            debug(s"     === Bind @@@")
          case Alternative(alts) =>
            debug(s"     @@@ Alternative(alts) @@@")
            log(s"     byte: ALTERNATIVE")
            writeByte(ALTERNATIVE)
            withLength { alts.foreach(pickleTree) }
            debug(s"     === Alternative @@@")
//          case UnApply(fun, implicits, patterns) =>
//            debug(s"     @@@ UnApply(fun, implicits, patterns) @@@")
//            log(s"     byte: UNAPPLY")
//            writeByte(UNAPPLY)
//            debug(s"fun: $fun")
//            implicits foreach { x => debug(s"implicit: $x") }
//            patterns foreach { p => debug(s"pattern: $p") }
//            debug(s"tree.tpe: ${tree.tpe}")
//            withLength {
//              pickleTree(fun)
//              for (implicitArg <- implicits) {
//                log(s"     byte: IMPLICITarg")
//                writeByte(IMPLICITarg)
//                pickleTree(implicitArg)
//              }
//              pickleType(tree.tpe)
//              patterns.foreach(pickleTree)
//            }
//            debug(s"     === UnApply @@@")
          case tree: ValDef =>
            debug(s"     @@@ ValDef @@@")
            log(s"     byte: VALDEF")
            pickleDef(VALDEF, tree.symbol, tree.tpt, tree.rhs)
            debug(s"     === ValDef @@@")
          case tree: DefDef =>
            debug(s"     @@@ DefDef @@@")
            def pickleAllParams = {
              pickleParams(tree.tparams)
              for (vparams <- tree.vparamss) {
                log(s"     byte: PARAMS")
                writeByte(PARAMS)
                withLength { pickleParams(vparams) }
              }
            }
            log(s"     byte: DEFDEF")
            pickleDef(DEFDEF, tree.symbol, tree.tpt, tree.rhs, pickleAllParams)
            debug(s"     === DefDef @@@")
          case tree: TypeDef =>
            debug(s"     @@@ TypeDef @@@")
            log(s"     byte: TYPEDEF")
            pickleDef(TYPEDEF, tree.symbol, tree.rhs)
            debug(s"     === TypeDef @@@")
          case tree: ClassDef =>
            debug(s"     @@@ TypeDef (ClassDef) @@@")
            log(s"     byte: TYPEDEF")
            pickleDef(TYPEDEF, tree.symbol, tree.impl)
            debug(s"     === TypeDef @@@")
          case tree: Template =>
            debug(s"     @@@ Template @@@")
            //TODO - see treeInfo
            val primaryCtr = treeInfo.firstConstructor(tree.body)
            val firstCtrArgs = treeInfo.firstConstructorArgs(tree.body)
            debug(s"primaryCtr: ${showRaw(primaryCtr)}")
            debug(s"firstCtrArgs: $firstCtrArgs")
            registerDef(tree.symbol)
            log(s"     byte: TEMPLATE")
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
                log(s"     byte: SELFDEF")
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
            debug(s"     === Template @@@")
          case Import(expr, selectors) =>
            debug(s"     @@@ Import @@@")
            log(s"     byte: IMPORT")
            writeByte(IMPORT)
            withLength {
              pickleTree(expr)
//              selectors foreach {
//                case Pair(Ident(from), Ident(to)) =>
//                  log(s"     byte: RENAMED")
//                  writeByte(RENAMED)
//                  withLength { pickleName(from); pickleName(to) }
//                case Ident(name) =>
//                  log(s"     byte: IMPORTED")
//                  writeByte(IMPORTED)
//                  pickleName(name)
//              }
            }
            debug(s"     === Import @@@")
          case PackageDef(pid, stats) =>
            debug(s"     @@@ PackageDef @@@")
            log(s"     byte: PACKAGE")
            writeByte(PACKAGE)
            withLength { pickleType(pid.tpe); pickleStats(stats) }
            debug(s"     === PackageDef @@@")
        }; debug("")
      } catch {
        case ex: AssertionError =>
          println(s"error when pickling tree $tree")
          throw ex
      }

      def pickleDef(tag: Int, sym: Symbol, tpt: Tree, rhs: Tree = EmptyTree, pickleParams: => Unit = ()) = {
        assert(symRefs(sym) == NoAddr)
        registerDef(sym)
        log(s"     byte: $tag")
        writeByte(tag)
        withLength {
          pickleName(sym.name)
          pickleParams
          debug(s"   tpt(scala): ${showRaw(tpt)}")
          tpt match {
            case tpt: TypeTree => 
              pickleTpt(tpt)
            case _             => 
              pickleTree(tpt)
          }
          pickleTreeUnlessEmpty(rhs)
          //TODO - check correctness of pickled modifiers
          pickleModifiers(sym)
        }
      }

      def pickleParam(tree: Tree): Unit = {
        debug(s"===> pickleParam");
        tree match {
          case tree: ValDef  => 
            log(s"     byte: PARAM");
            pickleDef(PARAM, tree.symbol, tree.tpt)
          case tree: DefDef  => 
            log(s"     byte: PARAM");
            pickleDef(PARAM, tree.symbol, tree.tpt, tree.rhs)
          case tree: TypeDef => 
            log(s"     byte: TYPEPARAM");
            pickleDef(TYPEPARAM, tree.symbol, tree.rhs)
        }
        debug(s"<=== pickleParam");
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
        debug(s"===> pickleModifiers");
        log(s"     byte: pickleModifiers")
//        import Flags._
        val flags = sym.flags
        val privateWithin = sym.privateWithin
        if (privateWithin.exists) {
          writeByte(if (sym.isProtected /*flags is Protected*/) PROTECTEDqualified else PRIVATEqualified)
          //TODO - fix it
//          debug(s"     privateWithin.typeRef: ${privateWithin.typeRef}");
//          pickleType(privateWithin.typeRef)
        }
        if (sym.isPrivate /*flags is Private*/) writeByte(PRIVATE)
        if (sym.isProtected /*flags is Protected*/) if (!privateWithin.exists) writeByte(PROTECTED)
        if (sym.isFinal /*flags is Final*/) writeByte(FINAL)
        if (sym.isCase /*flags is Case*/) writeByte(CASE)
        if (sym.isOverride /*flags is Override*/) writeByte(OVERRIDE)
//        if (flags is Inline) writeByte(INLINE)
//        if (flags is JavaStatic) writeByte(STATIC)
        if (sym.isModule /*flags is Module*/) writeByte(OBJECT)
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
        debug(s"<=== pickleModifiers");
      }

      def pickleAnnotation(ann: Annotation) = {
        debug(s"===> pickleAnnotation: $ann");
        log(s"     byte: ANNOTATION");
        debug(s"     ann.tree: ${ann.tree}");
//        debug(s"     ann.symbol.typeRef: ${ann.symbol.typeRef}");
        writeByte(ANNOTATION)
//        withLength { pickleType(ann.symbol.typeRef); pickleTree(ann.tree) }
        debug(s"<=== pickleAnnotation");
      }

      def updateMapWithDeltas[T](mp: collection.mutable.Map[T, Addr]) =
        for (key <- mp.keysIterator.toBuffer[T]) mp(key) = adjusted(mp(key))
      
      trees.foreach(tree => if (!tree.isEmpty) pickleTree(tree))
      assert(forwardSymRefs.isEmpty, s"unresolved symbols: ${forwardSymRefs.keySet.toList}%, %")
      compactify()
      updateMapWithDeltas(symRefs)
    }
  }
}
