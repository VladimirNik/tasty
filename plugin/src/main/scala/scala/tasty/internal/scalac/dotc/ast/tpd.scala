package scala.tasty.internal.scalac
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Denotations._, Decorators._
import config.Printers._
import collection.mutable

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

  /** A tree representing the same reference as the given type */
  def ref(tp: NamedType)(implicit ctx: Context): Tree = ???

  def ref(sym: Symbol)(implicit ctx: Context): Tree = ???

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
  }
}

