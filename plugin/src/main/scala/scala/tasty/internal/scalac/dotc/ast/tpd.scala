package scala.tasty.internal.scalac
package dotc
package ast

//import dotty.tools.dotc.transform.ExplicitOuter
//import dotty.tools.dotc.typer.ProtoTypes.FunProtoTyped
//import transform.SymUtils._
import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import /*SymDenotations._, */Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import /*Denotations._, */Decorators._ /*, DenotTransformers._*/
import config.Printers._
//import typer.Mode
import collection.mutable
//import typer.ErrorReporting._

import scala.annotation.tailrec

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] /* with TypedTreeInfo */{

  def Modifiers(sym: Symbol)(implicit ctx: Context): Modifiers = Modifiers(
    sym.flags & ModifierFlags,
    if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
    sym.annotations map (_.tree))

  def Ident(tp: NamedType)(implicit ctx: Context): Ident = ???

  def Select(qualifier: Tree, name: Name)(implicit ctx: Context): Select = ???

  def SelectFromTypeTree(qualifier: Tree, name: Name)(implicit ctx: Context): SelectFromTypeTree = ???

  def SelectFromTypeTree(qualifier: Tree, tp: NamedType)(implicit ctx: Context): SelectFromTypeTree = ???

  def This(cls: ClassSymbol)(implicit ctx: Context): This = ???

  def Super(qual: Tree, mix: TypeName, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol)(implicit ctx: Context): Super = ???

  def Apply(fn: Tree, args: List[Tree])(implicit ctx: Context): Apply = ???

  def TypeApply(fn: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = ???

  def Literal(const: Constant)(implicit ctx: Context): Literal = ???

  def unitLiteral(implicit ctx: Context): Literal =
    Literal(Constant(()))

  def New(tpt: Tree)(implicit ctx: Context): New = ???

  def New(tp: Type)(implicit ctx: Context): New = New(TypeTree(tp))

  def Pair(left: Tree, right: Tree)(implicit ctx: Context): Pair = ???

  def Typed(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed = ???

  def NamedArg(name: Name, arg: Tree)(implicit ctx: Context) = ???

  def Assign(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign = ???

  def Block(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = ???

  /** Join `stats` in front of `expr` creating a new block if necessary */
//  def seq(stats: List[Tree], expr: Tree)(implicit ctx: Context): Tree =
//    if (stats.isEmpty) expr
//    else expr match {
//      case Block(estats, eexpr) => cpy.Block(expr)(stats ::: estats, eexpr)
//      case _ => Block(stats, expr)
//    }

  def If(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If = ???

  def Closure(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure = ???

  /** A function def
   *
   *    vparams => expr
   *
   *  gets expanded to
   *
   *    { def $anonfun(vparams) = expr; Closure($anonfun) }
   *
   *  where the closure's type is the target type of the expression (FunctionN, unless
   *  otherwise specified).
   */
  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targs: List[Tree] = Nil, targetType: Type = NoType)(implicit ctx: Context): Block = {
    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
    val call =
      if (targs.isEmpty) Ident(TermRef(NoPrefix, meth))
      else TypeApply(Ident(TermRef(NoPrefix, meth)), targs)
    Block(
      DefDef(meth, rhsFn) :: Nil,
      Closure(Nil, call, targetTpt))
  }

  def CaseDef(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef = ???

  def Match(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match = ???

  def Return(expr: Tree, from: Tree)(implicit ctx: Context): Return = ???

  def Try(block: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try = ???

  def SeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral = ???

  def SeqLiteral(tpe: Type, elems: List[Tree])(implicit ctx: Context): SeqLiteral = ???

  def JavaSeqLiteral(elems: List[Tree])(implicit ctx: Context): SeqLiteral = ???

  def TypeTree(original: Tree)(implicit ctx: Context): TypeTree =
    TypeTree(original.tpe, original)

  def TypeTree(tp: Type, original: Tree = EmptyTree)(implicit ctx: Context): TypeTree = ???

  def SingletonTypeTree(ref: Tree)(implicit ctx: Context): SingletonTypeTree = ???

  def AndTypeTree(left: Tree, right: Tree)(implicit ctx: Context): AndTypeTree = ???

  def OrTypeTree(left: Tree, right: Tree)(implicit ctx: Context): OrTypeTree = ???

  // RefinedTypeTree is missing, handled specially in Typer and Unpickler.

  def AppliedTypeTree(tycon: Tree, args: List[Tree])(implicit ctx: Context): AppliedTypeTree = ???

  def ByNameTypeTree(result: Tree)(implicit ctx: Context): ByNameTypeTree = ???

  def TypeBoundsTree(lo: Tree, hi: Tree)(implicit ctx: Context): TypeBoundsTree = ???

  def Bind(sym: TermSymbol, body: Tree)(implicit ctx: Context): Bind = ???

  def Alternative(trees: List[Tree])(implicit ctx: Context): Alternative = ???

  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree], proto: Type)(implicit ctx: Context): UnApply = ???

  def ValDef(sym: TermSymbol, rhs: LazyTree = EmptyTree)(implicit ctx: Context): ValDef = ???

  def SyntheticValDef(name: TermName, rhs: Tree)(implicit ctx: Context): ValDef = ???

  def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef = ???

  def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = ???

//  def polyDefDef(sym: TermSymbol, rhsFn: List[Type] => List[List[Tree]] => Tree)(implicit ctx: Context): DefDef = {
//    val (tparams, mtp) = sym.info match {
//      case tp: PolyType =>
//        val tparams = ctx.newTypeParams(sym, tp.paramNames, EmptyFlags, tp.instantiateBounds)
//        (tparams, tp.instantiate(tparams map (_.typeRef)))
//      case tp => (Nil, tp)
//    }
//
//    def valueParamss(tp: Type): (List[List[TermSymbol]], Type) = tp match {
//      case tp @ MethodType(paramNames, paramTypes) =>
//        def valueParam(name: TermName, info: Type): TermSymbol = {
//          val maybeImplicit = if (tp.isInstanceOf[ImplicitMethodType]) Implicit else EmptyFlags
//          ctx.newSymbol(sym, name, TermParam | maybeImplicit, info)
//        }
//        val params = (paramNames, paramTypes).zipped.map(valueParam)
//        val (paramss, rtp) = valueParamss(tp.instantiate(params map (_.termRef)))
//        (params :: paramss, rtp)
//      case tp => (Nil, tp.widenExpr)
//    }
//    val (vparamss, rtp) = valueParamss(mtp)
//    val targs = tparams map (_.typeRef)
//    val argss = vparamss.nestedMap(vparam => Ident(vparam.termRef))
//    ta.assignType(
//      untpd.DefDef(
//        sym.name,
//        tparams map TypeDef,
//        vparamss.nestedMap(ValDef(_)),
//        TypeTree(rtp),
//        rhsFn(targs)(argss)),
//      sym)
//  }

  def TypeDef(sym: TypeSymbol)(implicit ctx: Context): TypeDef = ???

  def ClassDef(cls: ClassSymbol, constr: DefDef, body: List[Tree], superArgs: List[Tree] = Nil)(implicit ctx: Context): TypeDef = ???

  /** An anonymous class
   *
   *      new parents { forwarders }
   *
   *  where `forwarders` contains forwarders for all functions in `fns`.
   *  @param parents    a non-empty list of class types
   *  @param fns        a non-empty of functions for which forwarders should be defined in the class.
   *  The class has the same owner as the first function in `fns`.
   *  Its position is the union of all functions in `fns`.
   */
  def AnonClass(parents: List[Type], fns: List[TermSymbol], methNames: List[TermName])(implicit ctx: Context): Block = ???

  // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
  def WhileDo(owner: Symbol, cond: Tree, body: List[Tree])(implicit ctx: Context): Tree = ???

  def Import(expr: Tree, selectors: List[untpd.Tree])(implicit ctx: Context): Import = ???

  def PackageDef(pid: RefTree, stats: List[Tree])(implicit ctx: Context): PackageDef = ???

  def Annotated(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated = ???

  def Throw(expr: Tree)(implicit ctx: Context): Tree = ???

  // ------ Making references ------------------------------------------------------

//  def prefixIsElidable(tp: NamedType)(implicit ctx: Context) = {
//    def test(implicit ctx: Context) = tp.prefix match {
//      case NoPrefix =>
//        true
//      case pre: ThisType =>
//        pre.cls.isStaticOwner ||
//          tp.symbol.is(ParamOrAccessor) && !pre.cls.is(Trait) && ctx.owner.enclosingClass == pre.cls
//          // was ctx.owner.enclosingClass.derivesFrom(pre.cls) which was not tight enough
//          // and was spuriously triggered in case inner class would inherit from outer one
//          // eg anonymous TypeMap inside TypeMap.andThen
//      case pre: TermRef =>
//        pre.symbol.is(Module) && pre.symbol.isStatic
//      case _ =>
//        false
//    }
//    try test || tp.symbol.is(JavaStatic)
//    catch { // See remark in SymDenotations#accessWithin
//      case ex: NotDefinedHere => test(ctx.addMode(Mode.FutureDefsOK))
//    }
//  }

//  def needsSelect(tp: Type)(implicit ctx: Context) = tp match {
//    case tp: TermRef => !prefixIsElidable(tp)
//    case _ => false
//  }

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): Tree = ???

  def ref(sym: Symbol)(implicit ctx: Context): Tree = ???

//  private def followOuterLinks(t: Tree)(implicit ctx: Context) = t match {
//    //TODO - we work before erasure
////    case t: This if ctx.erasedTypes && !(t.symbol == ctx.owner.enclosingClass || t.symbol.isStaticOwner) =>
////       //after erasure outer paths should be respected
////      new ExplicitOuter.OuterOps(ctx).path(t.tpe.widen.classSymbol)
//    case t =>
//      t
//  }

//  def singleton(tp: Type)(implicit ctx: Context): Tree = tp match {
//    case tp: TermRef => ref(tp)
//    case tp: ThisType => This(tp.cls)
//    case SuperType(qual, _) => singleton(qual)
//    case ConstantType(value) => Literal(value)
//  }
//
//  /** A tree representing a `newXYZArray` operation of the right
//   *  kind for the given element type in `typeArg`. No type arguments or
//   *  `length` arguments are given.
//   */
//  def newArray(typeArg: Tree, pos: Position)(implicit ctx: Context): Tree = {
//    val elemType = typeArg.tpe
//    val elemClass = elemType.classSymbol
//    def newArr(kind: String) =
//      ref(defn.DottyArraysModule).select(s"new${kind}Array".toTermName).withPos(pos)
//    if (TypeErasure.isUnboundedGeneric(elemType))
//      newArr("Generic").appliedToTypeTrees(typeArg :: Nil)
//    else if (elemClass.isPrimitiveValueClass)
//      newArr(elemClass.name.toString)
//    else
//      newArr("Ref").appliedToTypeTrees(
//        TypeTree(defn.ArrayType(elemType)).withPos(typeArg.pos) :: Nil)
//  }

  // ------ Creating typed equivalents of trees that exist only in untyped form -------

  /** new C(args), calling the primary constructor of C */
  def New(tp: Type, args: List[Tree])(implicit ctx: Context): Apply = ???

  /** new C(args), calling given constructor `constr` of C */
  def New(tp: Type, constr: TermSymbol, args: List[Tree])(implicit ctx: Context): Apply = ???

  /** An object def
   *
   *     object obs extends parents { decls }
   *
   *  gets expanded to
   *
   *     <module> val obj = new obj$
   *     <module> class obj$ extends parents { this: obj.type => decls }
   *
   *  (The following no longer applies:
   *  What's interesting here is that the block is well typed
   *  (because class obj$ is hoistable), but the type of the `obj` val is
   *  not expressible. What needs to happen in general when
   *  inferring the type of a val from its RHS, is: if the type contains
   *  a class that has the val itself as owner, then that class
   *  is remapped to have the val's owner as owner. Remapping could be
   *  done by cloning the class with the new owner and substituting
   *  everywhere in the tree. We know that remapping is safe
   *  because the only way a local class can appear in the RHS of a val is
   *  by being hoisted outside of a block, and the necessary checks are
   *  done at this point already.
   *
   *  On the other hand, for method result type inference, if the type of
   *  the RHS of a method contains a class owned by the method, this would be
   *  an error.)
   */
  def ModuleDef(sym: TermSymbol, body: List[Tree])(implicit ctx: Context): tpd.Thicket = ???

  /** A `_' with given type */
  def Underscore(tp: Type)(implicit ctx: Context) = ??? //untpd.Ident(nme.WILDCARD).withType(tp)

  def defaultValue(tpe: Types.Type)(implicit ctx: Context) = ???

//  private class FindLocalDummyAccumulator(cls: ClassSymbol)(implicit ctx: Context) extends TreeAccumulator[Symbol] {
//    def apply(sym: Symbol, tree: Tree)(implicit ctx: Context) =
//      if (sym.exists) sym
//      else if (tree.isDef) {
//        val owner = tree.symbol.owner
//        if (owner.isLocalDummy && owner.owner == cls) owner
//        else if (owner == cls) foldOver(sym, tree)
//        else sym
//      } else foldOver(sym, tree)
//  }
//
//  implicit class modsDeco(mdef: MemberDef)(implicit ctx: Context) extends ModsDeco {
//    def mods = if (mdef.hasType) Modifiers(mdef.symbol) else mdef.rawMods
//  }

//  override val cpy = new TypedTreeCopier
//
//  class TypedTreeCopier extends TreeCopier {
//    def postProcess(tree: Tree, copied: untpd.Tree): copied.ThisTree[Type] =
//      copied.withTypeUnchecked(tree.tpe)
//    def postProcess(tree: Tree, copied: untpd.MemberDef): copied.ThisTree[Type] =
//      copied.withTypeUnchecked(tree.tpe)
//
//    override def Select(tree: Tree)(qualifier: Tree, name: Name)(implicit ctx: Context): Select = {
//      val tree1 = untpd.cpy.Select(tree)(qualifier, name)
//      tree match {
//        case tree: Select if qualifier.tpe eq tree.qualifier.tpe =>
//          tree1.withTypeUnchecked(tree.tpe)
//        case _ => tree.tpe match {
//          case tpe: NamedType => tree1.withType(tpe.derivedSelect(qualifier.tpe))
//          case _ => tree1.withTypeUnchecked(tree.tpe)
//        }
//      }
//    }
//
//    override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): Apply = ???
//      // Note: Reassigning the original type if `fun` and `args` have the same types as before
//      // does not work here: The computed type depends on the widened function type, not
//      // the function type itself. A treetransform may keep the function type the
//      // same but its widened type might change.
//
//    override def TypeApply(tree: Tree)(fun: Tree, args: List[Tree])(implicit ctx: Context): TypeApply = ???
//      // Same remark as for Apply
//
//    override def Literal(tree: Tree)(const: Constant)(implicit ctx: Context): Literal = ???
//
//    override def New(tree: Tree)(tpt: Tree)(implicit ctx: Context): New = ???
//
//    override def Pair(tree: Tree)(left: Tree, right: Tree)(implicit ctx: Context): Pair = ???
//
//    override def Typed(tree: Tree)(expr: Tree, tpt: Tree)(implicit ctx: Context): Typed = ???
//
//    override def NamedArg(tree: Tree)(name: Name, arg: Tree)(implicit ctx: Context): NamedArg = ???
//
//    override def Assign(tree: Tree)(lhs: Tree, rhs: Tree)(implicit ctx: Context): Assign = ???
//
//    override def Block(tree: Tree)(stats: List[Tree], expr: Tree)(implicit ctx: Context): Block = ???
//    
//    override def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree)(implicit ctx: Context): If = ???
//
//    override def Closure(tree: Tree)(env: List[Tree], meth: Tree, tpt: Tree)(implicit ctx: Context): Closure = ???
//      // Same remark as for Apply
//
//    override def Match(tree: Tree)(selector: Tree, cases: List[CaseDef])(implicit ctx: Context): Match = ???
//
//    override def CaseDef(tree: Tree)(pat: Tree, guard: Tree, body: Tree)(implicit ctx: Context): CaseDef = ???
//
//    override def Return(tree: Tree)(expr: Tree, from: Tree)(implicit ctx: Context): Return = ???
//
//    override def Try(tree: Tree)(expr: Tree, cases: List[CaseDef], finalizer: Tree)(implicit ctx: Context): Try = ???
//
//    override def SeqLiteral(tree: Tree)(elems: List[Tree])(implicit ctx: Context): SeqLiteral = ???
//
//    override def Annotated(tree: Tree)(annot: Tree, arg: Tree)(implicit ctx: Context): Annotated = ???
//
//    override def If(tree: If)(cond: Tree = tree.cond, thenp: Tree = tree.thenp, elsep: Tree = tree.elsep)(implicit ctx: Context): If =
//      If(tree: Tree)(cond, thenp, elsep)
//    override def Closure(tree: Closure)(env: List[Tree] = tree.env, meth: Tree = tree.meth, tpt: Tree = tree.tpt)(implicit ctx: Context): Closure =
//      Closure(tree: Tree)(env, meth, tpt)
//    override def CaseDef(tree: CaseDef)(pat: Tree = tree.pat, guard: Tree = tree.guard, body: Tree = tree.body)(implicit ctx: Context): CaseDef =
//      CaseDef(tree: Tree)(pat, guard, body)
//    override def Try(tree: Try)(expr: Tree = tree.expr, cases: List[CaseDef] = tree.cases, finalizer: Tree = tree.finalizer)(implicit ctx: Context): Try =
//      Try(tree: Tree)(expr, cases, finalizer)
//  }

  implicit class TreeOps[ThisTree <: tpd.Tree](val tree: ThisTree) extends AnyVal {

    def isValue(implicit ctx: Context): Boolean =
      tree.isTerm && tree.tpe.widen.isValueType

    def isValueOrPattern(implicit ctx: Context) =
      tree.isValue || tree.isPattern

    def isValueType: Boolean =
      tree.isType && tree.tpe.isValueType

    def isInstantiation: Boolean = tree match {
      case Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
      case _ => false
    }

//    def shallowFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context) =
//      new ShallowFolder(op).apply(z, tree)
//
//    def deepFold[T](z: T)(op: (T, tpd.Tree) => T)(implicit ctx: Context) =
//      new DeepFolder(op).apply(z, tree)
//
//    def find[T](pred: (tpd.Tree) => Boolean)(implicit ctx: Context): Option[tpd.Tree] =
//      shallowFold[Option[tpd.Tree]](None)((accum, tree) => if (pred(tree)) Some(tree) else accum)
//
//    def subst(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): ThisTree =
//      new TreeTypeMap(substFrom = from, substTo = to).apply(tree)
//
//    /** Change owner from `from` to `to`. If `from` is a weak owner, also change its
//     *  owner to `to`, and continue until a non-weak owner is reached.
//     */
//    def changeOwner(from: Symbol, to: Symbol)(implicit ctx: Context): ThisTree = {
//      def loop(from: Symbol, froms: List[Symbol], tos: List[Symbol]): ThisTree = {
//        if (from.isWeakOwner && !from.owner.isClass)
//          loop(from.owner, from :: froms, to :: tos)
//        else {
//          //println(i"change owner ${from :: froms}%, % ==> $tos of $tree")
//          new TreeTypeMap(oldOwners = from :: froms, newOwners = tos)(ctx.withMode(Mode.FutureDefsOK)).apply(tree)
//        }
//      }
//      loop(from, Nil, to :: Nil)
//    }
//
//    /** After phase `trans`, set the owner of every definition in this tree that was formerly
//     *  owner by `from` to `to`.
//     */
//    def changeOwnerAfter(from: Symbol, to: Symbol, trans: DenotTransformer)(implicit ctx: Context): ThisTree = {
//      assert(ctx.phase == trans.next)
//      val traverser = new TreeTraverser {
//        def traverse(tree: Tree)(implicit ctx: Context) = tree match {
//          case tree: DefTree =>
//            val sym = tree.symbol
//            if (sym.denot(ctx.withPhase(trans)).owner == from) {
//              val d = sym.copySymDenotation(owner = to)
//              d.installAfter(trans)
//              d.transformAfter(trans, d => if (d.owner eq from) d.copySymDenotation(owner = to) else d)
//            }
//            if (sym.isWeakOwner) traverseChildren(tree)
//          case _ =>
//            traverseChildren(tree)
//        }
//      }
//      traverser.traverse(tree)
//      tree
//    }

    /** A select node with the given selector name and a computed type */
//    def select(name: Name)(implicit ctx: Context): Select =
//      Select(tree, name)
//
//    /** A select node with the given type */
//    def select(tp: NamedType)(implicit ctx: Context): Select =
//      untpd.Select(tree, tp.name).withType(tp)
//
//    /** A select node that selects the given symbol. Note: Need to make sure this
//     *  is in fact the symbol you would get when you select with the symbol's name,
//     *  otherwise a data race may occur which would be flagged by -Yno-double-bindings.
//     */
//    def select(sym: Symbol)(implicit ctx: Context): Select =
//      untpd.Select(tree, sym.name).withType(
//        TermRef.withSigAndDenot(tree.tpe, sym.name.asTermName, sym.signature, sym.denot.asSeenFrom(tree.tpe)))
//
//    /** A select node with the given selector name and signature and a computed type */
//    def selectWithSig(name: Name, sig: Signature)(implicit ctx: Context): Tree =
//      untpd.SelectWithSig(tree, name, sig)
//        .withType(TermRef.withSig(tree.tpe, name.asTermName, sig))

    /** A select node with selector name and signature taken from `sym`.
     *  Note: Use this method instead of select(sym) if the referenced symbol
     *  might be overridden in the type of the qualifier prefix. See note
     *  on select(sym: Symbol).
     */
//    def selectWithSig(sym: Symbol)(implicit ctx: Context): Tree =
//      selectWithSig(sym.name, sym.signature)

    /** A unary apply node with given argument: `tree(arg)` */
    def appliedTo(arg: Tree)(implicit ctx: Context): Tree =
      appliedToArgs(arg :: Nil)

    /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
    def appliedTo(arg: Tree, args: Tree*)(implicit ctx: Context): Tree =
      appliedToArgs(arg :: args.toList)

    /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
    def appliedToArgs(args: List[Tree])(implicit ctx: Context): Apply =
      Apply(tree, args)

    /** The current tree applied to given argument lists:
     *  `tree (argss(0)) ... (argss(argss.length -1))`
     */
    def appliedToArgss(argss: List[List[Tree]])(implicit ctx: Context): Tree =
      ((tree: Tree) /: argss)(Apply(_, _))

    /** The current tree applied to (): `tree()` */
    def appliedToNone(implicit ctx: Context): Apply = appliedToArgs(Nil)

    /** The current tree applied to given type argument: `tree[targ]` */
    def appliedToType(targ: Type)(implicit ctx: Context): Tree =
      appliedToTypes(targ :: Nil)

    /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
    def appliedToTypes(targs: List[Type])(implicit ctx: Context): Tree =
      appliedToTypeTrees(targs map (TypeTree(_)))

    /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
    def appliedToTypeTrees(targs: List[Tree])(implicit ctx: Context): Tree =
      if (targs.isEmpty) tree else TypeApply(tree, targs)

    /** Apply to `()` unless tree's widened type is parameterless */
    def ensureApplied(implicit ctx: Context): Tree =
      if (tree.tpe.widen.isParameterless) tree else tree.appliedToNone

    /** `tree.isInstanceOf[tp]` */
//    def isInstance(tp: Type)(implicit ctx: Context): Tree =
//      tree.select(defn.Any_isInstanceOf).appliedToType(tp)
//
//    /** tree.asInstanceOf[`tp`] */
//    def asInstance(tp: Type)(implicit ctx: Context): Tree = {
//      assert(tp.isValueType, i"bad cast: $tree.asInstanceOf[$tp]")
//      tree.select(defn.Any_asInstanceOf).appliedToType(tp)
//    }
//
//    /** `tree.asInstanceOf[tp]` unless tree's type already conforms to `tp` */
//    def ensureConforms(tp: Type)(implicit ctx: Context): Tree =
//      if (tree.tpe <:< tp) tree else asInstance(tp)
//
//    /** If inititializer tree is `_', the default value of its type,
//     *  otherwise the tree itself.
//     */
//    def wildcardToDefault(implicit ctx: Context) =
//      if (isWildcardArg(tree)) defaultValue(tree.tpe) else tree
//
//    /** `this && that`, for boolean trees `this`, `that` */
//    def and(that: Tree)(implicit ctx: Context): Tree =
//      tree.select(defn.Boolean_&&).appliedTo(that)
//
//    /** `this || that`, for boolean trees `this`, `that` */
//    def or(that: Tree)(implicit ctx: Context): Tree =
//      tree.select(defn.Boolean_||).appliedTo(that)

    /** The translation of `tree = rhs`.
     *  This is either the tree as an assignment, to a setter call.
     */
//    def becomes(rhs: Tree)(implicit ctx: Context): Tree =
//      if (tree.symbol is Method) {
//        val setr = tree match {
//          case Ident(_) =>
//            val setter = tree.symbol.setter
//            assert(setter.exists, tree.symbol.showLocated)
//            ref(tree.symbol.setter)
//          case Select(qual, _) => qual.select(tree.symbol.setter)
//        }
//        setr.appliedTo(rhs)
//      }
//      else Assign(tree, rhs)
//
//    // --- Higher order traversal methods -------------------------------
//
//    /** Apply `f` to each subtree of this tree */
//    def foreachSubTree(f: Tree => Unit)(implicit ctx: Context): Unit = {
//      val traverser = new TreeTraverser {
//        def traverse(tree: Tree)(implicit ctx: Context) = foldOver(f(tree), tree)
//      }
//      traverser.traverse(tree)
//    }
//
//    /** Is there a subtree of this tree that satisfies predicate `p`? */
//    def existsSubTree(p: Tree => Boolean)(implicit ctx: Context): Boolean = {
//      val acc = new TreeAccumulator[Boolean] {
//        def apply(x: Boolean, t: Tree)(implicit ctx: Context) = x || p(t) || foldOver(x, t)
//      }
//      acc(false, tree)
//    }

    /** All subtrees of this tree that satisfy predicate `p`. */
//    def filterSubTrees(f: Tree => Boolean)(implicit ctx: Context): List[Tree] = {
//      val buf = new mutable.ListBuffer[Tree]
//      foreachSubTree { tree => if (f(tree)) buf += tree }
//      buf.toList
//    }
  }

//  implicit class ListOfTreeDecorator(val xs: List[tpd.Tree]) extends AnyVal {
//    def tpes: List[Type] = xs map (_.tpe)
//  }
//
//  // convert a numeric with a toXXX method
//  def primitiveConversion(tree: Tree, numericCls: Symbol)(implicit ctx: Context): Tree = {
//    val mname      = ("to" + numericCls.name).toTermName
//    val conversion = tree.tpe member mname
//    if (conversion.symbol.exists)
//      tree.select(conversion.symbol.termRef).ensureApplied
//    else if (tree.tpe.widen isRef numericCls)
//      tree
//    else {
//      ctx.warning(i"conversion from ${tree.tpe.widen} to ${numericCls.typeRef} will always fail at runtime.")
//      Throw(New(defn.ClassCastExceptionClass.typeRef, Nil)) withPos tree.pos
//    }
//  }

//  @tailrec
//  def sameTypes(trees: List[tpd.Tree], trees1: List[tpd.Tree]): Boolean = {
//    if (trees.isEmpty) trees.isEmpty
//    else if (trees1.isEmpty) trees.isEmpty
//    else (trees.head.tpe eq trees1.head.tpe) && sameTypes(trees.tail, trees1.tail)
//  }
//
//  def evalOnce(tree: Tree)(within: Tree => Tree)(implicit ctx: Context) = {
//    if (isIdempotentExpr(tree)) within(tree)
//    else {
//      val vdef = SyntheticValDef(ctx.freshName("ev$").toTermName, tree)
//      Block(vdef :: Nil, within(Ident(vdef.namedType)))
//    }
//  }
//
//  def runtimeCall(name: TermName, args: List[Tree])(implicit ctx: Context): Tree = {
//    Ident(defn.ScalaRuntimeModule.requiredMethod(name).termRef).appliedToArgs(args)
//  }

  /** An extractor that pulls out type arguments */
//  object MaybePoly {
//    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
//      case TypeApply(tree, targs) => Some(tree, targs)
//      case _ => Some(tree, Nil)
//    }
//  }
//
//  /** A traverser that passes the enclosing class or method as an argument
//   *  to the traverse method.
//   */
//  abstract class EnclosingMethodTraverser extends TreeAccumulator[Symbol] {
//    def traverse(enclMeth: Symbol, tree: Tree)(implicit ctx: Context): Unit
//    def apply(enclMeth: Symbol, tree: Tree)(implicit ctx: Context) = {
//      tree match {
//        case _: DefTree if tree.symbol.exists =>
//          traverse(tree.symbol.enclosingMethod, tree)
//        case _ =>
//          traverse(enclMeth, tree)
//      }
//      enclMeth
//    }
//  }

  // ensure that constructors are fully applied?
  // ensure that normal methods are fully applied?

}

