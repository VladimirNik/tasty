package scala.tasty.internal.scalac
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import language.higherKinds
import collection.mutable.ListBuffer

object untpd extends Trees.Instance[Untyped] {

  // ----- Tree cases that exist in untyped form only ------------------

  //TODO - these classes can be useful during the regeneration of Dotty trees in Dotty
//  trait OpTree extends Tree {
//    def op: Name
//    override def isTerm = op.isTermName
//    override def isType = op.isTypeName
//  }
//
//  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice */
//  case class TypedSplice(tree: tpd.Tree) extends ProxyTree {
//    def forwardTo = tree
//  }
//
//  /** mods object name impl */
//  case class ModuleDef(name: TermName, impl: Template)
//    extends MemberDef {
//    type ThisTree[-T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
//    def withName(name: Name)(implicit ctx: Context) = ???
//  }

//  case class ParsedTry(expr: Tree, handler: Tree, finalizer: Tree) extends TermTree
//
//  case class SymbolLit(str: String) extends TermTree
//  case class InterpolatedString(id: TermName, strings: List[Literal], elems: List[Tree]) extends TermTree
//  case class Function(args: List[Tree], body: Tree) extends Tree {
//    override def isTerm = body.isTerm
//    override def isType = body.isType
//  }
//  case class InfixOp(left: Tree, op: Name, right: Tree) extends OpTree
//  case class PostfixOp(od: Tree, op: Name) extends OpTree
//  case class PrefixOp(op: Name, od: Tree) extends OpTree
//  case class Parens(t: Tree) extends ProxyTree {
//    def forwardTo = t
//  }
//  case class Tuple(trees: List[Tree]) extends Tree {
//    override def isTerm = trees.isEmpty || trees.head.isTerm
//    override def isType = !isTerm
//  }
//  case class Throw(expr: Tree) extends TermTree
//  case class WhileDo(cond: Tree, body: Tree) extends TermTree
//  case class DoWhile(body: Tree, cond: Tree) extends TermTree
//  case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
//  case class ForDo(enums: List[Tree], body: Tree) extends TermTree
//  case class GenFrom(pat: Tree, expr: Tree) extends Tree
//  case class GenAlias(pat: Tree, expr: Tree) extends Tree
//  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree

  //TODO - these methods can be useful during regeneration of Tasty trees
  // ------ Creation methods for untyped only -----------------

//  def Ident(name: Name): Ident = new Ident(name)
//  def BackquotedIdent(name: Name): BackquotedIdent = new BackquotedIdent(name)
//  def Select(qualifier: Tree, name: Name): Select = new Select(qualifier, name)
//  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature): Select = new SelectWithSig(qualifier, name, sig)
//  def This(qual: TypeName): This = new This(qual)
//  def Super(qual: Tree, mix: TypeName): Super = new Super(qual, mix)
//  def Apply(fun: Tree, args: List[Tree]): Apply = new Apply(fun, args)
//  def TypeApply(fun: Tree, args: List[Tree]): TypeApply = new TypeApply(fun, args)
//  def Literal(const: Constant): Literal = new Literal(const)
//  def New(tpt: Tree): New = new New(tpt)
//  def Pair(left: Tree, right: Tree): Pair = new Pair(left, right)
//  def Typed(expr: Tree, tpt: Tree): Typed = new Typed(expr, tpt)
//  def NamedArg(name: Name, arg: Tree): NamedArg = new NamedArg(name, arg)
//  def Assign(lhs: Tree, rhs: Tree): Assign = new Assign(lhs, rhs)
//  def Block(stats: List[Tree], expr: Tree): Block = new Block(stats, expr)
//  def If(cond: Tree, thenp: Tree, elsep: Tree): If = new If(cond, thenp, elsep)
//  def Closure(env: List[Tree], meth: Tree, tpt: Tree): Closure = new Closure(env, meth, tpt)
//  def Match(selector: Tree, cases: List[CaseDef]): Match = new Match(selector, cases)
//  def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = new CaseDef(pat, guard, body)
//  def Return(expr: Tree, from: Tree): Return = new Return(expr, from)
//  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree): Try = new Try(expr, cases, finalizer)
//  def SeqLiteral(elems: List[Tree]): SeqLiteral = new SeqLiteral(elems)
//  def JavaSeqLiteral(elems: List[Tree]): JavaSeqLiteral = new JavaSeqLiteral(elems)
//  def TypeTree(original: Tree): TypeTree = new TypeTree(original)
//  def TypeTree() = new TypeTree(EmptyTree)
//  def SingletonTypeTree(ref: Tree): SingletonTypeTree = new SingletonTypeTree(ref)
//  def SelectFromTypeTree(qualifier: Tree, name: Name): SelectFromTypeTree = new SelectFromTypeTree(qualifier, name)
//  def AndTypeTree(left: Tree, right: Tree): AndTypeTree = new AndTypeTree(left, right)
//  def OrTypeTree(left: Tree, right: Tree): OrTypeTree = new OrTypeTree(left, right)
//  def RefinedTypeTree(tpt: Tree, refinements: List[Tree]): RefinedTypeTree = new RefinedTypeTree(tpt, refinements)
//  def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = new AppliedTypeTree(tpt, args)
//  def ByNameTypeTree(result: Tree): ByNameTypeTree = new ByNameTypeTree(result)
//  def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = new TypeBoundsTree(lo, hi)
//  def Bind(name: Name, body: Tree): Bind = new Bind(name, body)
//  def Alternative(trees: List[Tree]): Alternative = new Alternative(trees)
//  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree]): UnApply = new UnApply(fun, implicits, patterns)
//  def ValDef(name: TermName, tpt: Tree, rhs: LazyTree): ValDef = new ValDef(name, tpt, rhs)
//  def DefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: LazyTree): DefDef = new DefDef(name, tparams, vparamss, tpt, rhs)
//  def TypeDef(name: TypeName, rhs: Tree): TypeDef = new TypeDef(name, rhs)
//  def Template(constr: DefDef, parents: List[Tree], self: ValDef, body: LazyTreeList): Template = new Template(constr, parents, self, body)
//  def Import(expr: Tree, selectors: List[untpd.Tree]): Import = new Import(expr, selectors)
//  def PackageDef(pid: RefTree, stats: List[Tree]): PackageDef = new PackageDef(pid, stats)
//  def Annotated(annot: Tree, arg: Tree): Annotated = new Annotated(annot, arg)

  // ------ Additional creation methods for untyped only -----------------

//  /**     new pre.C[Ts](args1)...(args_n)
//   *  ==>
//   *      (new pre.C).<init>[Ts](args1)...(args_n)
//   */
//  def New(tpt: Tree, argss: List[List[Tree]])(implicit ctx: Context): Tree = ???
//
//  def Block(stat: Tree, expr: Tree): Block =
//    Block(stat :: Nil, expr)
//
//  def Apply(fn: Tree, arg: Tree): Apply =
//    Apply(fn, arg :: Nil)
//
//  def ensureApplied(tpt: Tree) = tpt match {
//    case _: Apply => tpt
//    case _ => Apply(tpt, Nil)
//  }
//
//  def AppliedTypeTree(tpt: Tree, arg: Tree): AppliedTypeTree =
//    AppliedTypeTree(tpt, arg :: Nil)
//
//  def TypeTree(tpe: Type): TypedSplice = ??? //TypedSplice(TypeTree().withTypeUnchecked(tpe))
//
//  def TypeDef(name: TypeName, tparams: List[TypeDef], rhs: Tree): TypeDef =
//    if (tparams.isEmpty) TypeDef(name, rhs) else new PolyTypeDef(name, tparams, rhs)
//
//  def unitLiteral = Literal(Constant(()))
//
//  def makeConstructor(tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
//    DefDef(nme.CONSTRUCTOR, tparams, vparamss, TypeTree(), rhs)
//
//  def emptyConstructor(implicit ctx: Context): DefDef =
//    makeConstructor(Nil, Nil)
//
//  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context) = ???
//
//  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context) = ts match {
//    case t :: Nil => Parens(t)
//    case _ => Tuple(ts)
//  }
//
//  def makeTuple(ts: List[Tree])(implicit ctx: Context) = ts match {
//    case t :: Nil => t
//    case _ => Tuple(ts)
//  }
//
//  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = EmptyModifiers)(implicit ctx: Context): ValDef = ???
//
//  def makeSyntheticParameter(n: Int = 1, tpt: Tree = TypeTree())(implicit ctx: Context): ValDef = ???
}
