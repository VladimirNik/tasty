package scala.tasty.internal
package dotc
package ast

trait TTrees {
  self: API =>

  import scala.tasty.internal.dotc.core._
  import Types._, Names._, Flags._, util.Positions._, Contexts._, Constants._, Symbols._
  import StdNames._
  import annotation.tailrec
  import language.higherKinds
  import collection.IndexedSeqOptimized
  import collection.immutable.IndexedSeq
  import collection.mutable.ListBuffer
  import annotation.unchecked.uncheckedVariance
  import language.implicitConversions

  //This outer object because we don't want to change code inside the pickling
  object ast {
    object Trees {
      type Untyped = Null

      case class Modifiers[-T >: Untyped](
        flags: FlagSet = EmptyFlags,
        privateWithin: TypeName = tpnme.EMPTY,
        annotations: List[Tree[T]] = Nil) extends Positioned with Cloneable {
      }

      abstract class Tree[-T >: Untyped] extends Positioned with Product with Cloneable {
        type ThisTree[T >: Untyped] <: Tree[T]

        private[this] var myTpe: T = _

        def tpe: T @uncheckedVariance = {
          if (myTpe == null)
            throw new UnAssignedTypeException(this)
          myTpe
        }

        def withType(tpe: T): ThisTree[Type] = {
          myTpe = tpe
          this.asInstanceOf[ThisTree[Type]]
        }

        private[this] var mySym: Symbol = NoSymbol

        final def symbol: Symbol = mySym

        //TODO - maybe it's better to reuse denot.symbol
        def withSymbol(sym: Symbol): ThisTree[Type] = {
          mySym = sym
          this.asInstanceOf[ThisTree[Type]]
        }

        def isEmpty: Boolean = false

        override def hashCode(): Int = System.identityHashCode(this)
        override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
      }

      class UnAssignedTypeException[T >: Untyped](tree: Tree[T]) extends RuntimeException {
        override def getMessage: String = s"type of $tree is not assigned"
      }

      trait TypTree[-T >: Untyped] extends Tree[T] {
        type ThisTree[-T >: Untyped] <: TypTree[T]
      }

      trait TermTree[-T >: Untyped] extends Tree[T] {
        type ThisTree[-T >: Untyped] <: TermTree[T]
      }

      trait PatternTree[-T >: Untyped] extends Tree[T] {
        type ThisTree[-T >: Untyped] <: PatternTree[T]
      }

      abstract class DenotingTree[-T >: Untyped] extends Tree[T] {
        type ThisTree[-T >: Untyped] <: DenotingTree[T]
      }

      abstract class ProxyTree[-T >: Untyped] extends Tree[T] {
        type ThisTree[-T >: Untyped] <: ProxyTree[T]
      }

      abstract class NameTree[-T >: Untyped] extends DenotingTree[T] {
        type ThisTree[-T >: Untyped] <: NameTree[T]
        def name: Name
      }

      abstract class RefTree[-T >: Untyped] extends NameTree[T] {
        type ThisTree[-T >: Untyped] <: RefTree[T]
        //    def qualifier: Tree[T]
      }

      trait DefTree[-T >: Untyped] extends DenotingTree[T] {
        type ThisTree[-T >: Untyped] <: DefTree[T]
      }

      abstract class MemberDef[-T >: Untyped] extends NameTree[T] with DefTree[T] {
        type ThisTree[-T >: Untyped] <: MemberDef[T]
        private[this] var myMods: Modifiers[T] = null
        protected def setMods(mods: Modifiers[T @uncheckedVariance]) = myMods = mods
      }

      trait ValOrDefDef[-T >: Untyped] extends MemberDef[T] with WithLazyField[Tree[T]] {
        def tpt: Tree[T]
        def rhs: Tree[T]
      }

      case class Ident[-T >: Untyped] private[ast] (name: Name) extends RefTree[T] {
        type ThisTree[-T >: Untyped] = Ident[T]
        //    def qualifier: Tree[T] = ???
      }

      case class Select[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name) extends RefTree[T] {
        type ThisTree[-T >: Untyped] = Select[T]
      }

      case class This[-T >: Untyped] private[ast] (qual: TypeName) extends DenotingTree[T] with TermTree[T] {
        type ThisTree[-T >: Untyped] = This[T]
      }

      case class Super[-T >: Untyped] private[ast] (qual: Tree[T], mix: TypeName) extends ProxyTree[T] with TermTree[T] {
        type ThisTree[-T >: Untyped] = Super[T]
      }

      abstract class GenericApply[-T >: Untyped] extends ProxyTree[T] with TermTree[T] {
        type ThisTree[-T >: Untyped] <: GenericApply[T]
        val fun: Tree[T]
        val args: List[Tree[T]]
      }

      case class Apply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]]) extends GenericApply[T] {
        type ThisTree[-T >: Untyped] = Apply[T]
      }

      case class TypeApply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]]) extends GenericApply[T] {
        type ThisTree[-T >: Untyped] = TypeApply[T]
      }

      case class Literal[-T >: Untyped] private[ast] (const: Constant) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Literal[T]
      }

      case class New[-T >: Untyped] private[ast] (tpt: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = New[T]
      }

      case class Pair[-T >: Untyped] private[ast] (left: Tree[T], right: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Pair[T]
      }

      case class Typed[-T >: Untyped] private[ast] (expr: Tree[T], tpt: Tree[T]) extends ProxyTree[T] with TermTree[T] {
        type ThisTree[-T >: Untyped] = Typed[T]
      }

      /** name = arg, in a parameter list */
      case class NamedArg[-T >: Untyped] private[ast] (name: Name, arg: Tree[T]) extends Tree[T] {
        type ThisTree[-T >: Untyped] = NamedArg[T]
      }

      /** name = arg, outside a parameter list */
      case class Assign[-T >: Untyped] private[ast] (lhs: Tree[T], rhs: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Assign[T]
      }

      case class Block[-T >: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Block[T]
      }

      case class If[-T >: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = If[T]
      }

      case class Closure[-T >: Untyped] private[ast] (env: List[Tree[T]], meth: Tree[T], tpt: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Closure[T]
      }

      case class Match[-T >: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Match[T]
      }

      case class CaseDef[-T >: Untyped] private[ast] (pat: Tree[T], guard: Tree[T], body: Tree[T]) extends Tree[T] {
        type ThisTree[-T >: Untyped] = CaseDef[T]
      }

      case class Return[-T >: Untyped] private[ast] (expr: Tree[T], from: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Return[T]
      }

      case class Try[-T >: Untyped] private[ast] (expr: Tree[T], cases: List[CaseDef[T]], finalizer: Tree[T]) extends TermTree[T] {
        type ThisTree[-T >: Untyped] = Try[T]
      }

      case class SeqLiteral[-T >: Untyped] private[ast] (elems: List[Tree[T]]) extends Tree[T] {
        type ThisTree[-T >: Untyped] = SeqLiteral[T]
      }

      class JavaSeqLiteral[T >: Untyped] private[ast] (elems: List[Tree[T]])
        extends SeqLiteral(elems) {
        override def toString = s"JavaSeqLiteral($elems)"
      }

      case class TypeTree[-T >: Untyped] private[ast] (original: Tree[T]) extends DenotingTree[T] with TypTree[T] {
        type ThisTree[-T >: Untyped] = TypeTree[T]
        //TODO - fix
        override def isEmpty = /*!hasType &&*/ original.isEmpty
        override def toString = s"TypeTree${ /*if (hasType) s"[$typeOpt]" else */ s"($original)"}"
      }

      /** ref.type */
      case class SingletonTypeTree[-T >: Untyped] private[ast] (ref: Tree[T]) extends DenotingTree[T] with TypTree[T] {
        type ThisTree[-T >: Untyped] = SingletonTypeTree[T]
      }

      /** tpt[args] */
      case class AppliedTypeTree[-T >: Untyped] private[ast] (tpt: Tree[T], args: List[Tree[T]]) extends ProxyTree[T] with TypTree[T] {
        type ThisTree[-T >: Untyped] = AppliedTypeTree[T]
      }

      /** => T */
      case class ByNameTypeTree[-T >: Untyped] private[ast] (result: Tree[T]) extends TypTree[T] {
        type ThisTree[-T >: Untyped] = ByNameTypeTree[T]
      }

      /** >: lo <: hi */
      case class TypeBoundsTree[-T >: Untyped] private[ast] (lo: Tree[T], hi: Tree[T]) extends TypTree[T] {
        type ThisTree[-T >: Untyped] = TypeBoundsTree[T]
      }

      /** name @ body */
      case class Bind[-T >: Untyped] private[ast] (name: Name, body: Tree[T]) extends NameTree[T] with DefTree[T] with PatternTree[T] {
        type ThisTree[-T >: Untyped] = Bind[T]
      }

      case class Alternative[-T >: Untyped] private[ast] (trees: List[Tree[T]]) extends PatternTree[T] {
        type ThisTree[-T >: Untyped] = Alternative[T]
      }

      case class UnApply[-T >: Untyped] private[ast] (fun: Tree[T], implicits: List[Tree[T]], patterns: List[Tree[T]]) extends PatternTree[T] {
        type ThisTree[-T >: Untyped] = UnApply[T]
      }

      case class ValDef[-T >: Untyped] private[ast] (name: TermName, tpt: Tree[T], val rhs: Tree[T]) extends ValOrDefDef[T] {
        type ThisTree[-T >: Untyped] = ValDef[T]
      }

      case class DefDef[-T >: Untyped] private[ast] (name: TermName, tparams: List[TypeDef[T]],
                                                     vparamss: List[List[ValDef[T]]], tpt: Tree[T], val rhs: Tree[T]) extends ValOrDefDef[T] {
        type ThisTree[-T >: Untyped] = DefDef[T]
      }

      /**
       * mods class name template     or
       *  mods trait name template     or
       *  mods type name = rhs   or
       *  mods type name >: lo <: hi, if rhs = TypeBoundsTree(lo, hi) & (lo ne hi)
       */
      case class TypeDef[-T >: Untyped] private[ast] (name: TypeName, rhs: Tree[T]) extends MemberDef[T] {
        type ThisTree[-T >: Untyped] = TypeDef[T]
        def isClassDef = rhs.isInstanceOf[Template[_]]
      }

      case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]]) extends DefTree[T] with WithLazyField[List[Tree[T]]] {
        type ThisTree[-T >: Untyped] = Template[T]
      }

      case class Import[-T >: Untyped] private[ast] (expr: Tree[T], selectors: List[Tree[Untyped]]) extends DenotingTree[T] {
        type ThisTree[-T >: Untyped] = Import[T]
      }

      case class PackageDef[-T >: Untyped] private[ast] (pid: RefTree[T], stats: List[Tree[T]]) extends ProxyTree[T] {
        type ThisTree[-T >: Untyped] = PackageDef[T]
      }

      case class Annotated[-T >: Untyped] private[ast] (annot: Tree[T], arg: Tree[T])
        extends ProxyTree[T] {
        type ThisTree[-T >: Untyped] = Annotated[T]
      }

      trait WithoutTypeOrPos[-T >: Untyped] extends Tree[T] {
        override def tpe: T @uncheckedVariance = NoType.asInstanceOf[T]
        override def pos = NoPosition
      }

      case class Thicket[-T >: Untyped](trees: List[Tree[T]]) extends Tree[T] with WithoutTypeOrPos[T] {
        type ThisTree[-T >: Untyped] = Thicket[T]
        override def isEmpty: Boolean = trees.isEmpty
        override def toString = if (isEmpty) "EmptyTree" else "Thicket(" + trees.mkString(", ") + ")"
      }

      class EmptyValDef[T >: Untyped] extends ValDef[T](
        nme.WILDCARD, genericEmptyTree[T], genericEmptyTree[T]) with WithoutTypeOrPos[T] {
        override def isEmpty: Boolean = true
        setMods(Modifiers[T](PrivateLocal))
      }

      val theEmptyTree: Thicket[Type] = Thicket(Nil)
      val theEmptyValDef = new EmptyValDef[Type]
      val theEmptyModifiers = new Modifiers()

      def genericEmptyValDef[T >: Untyped]: ValDef[T] = theEmptyValDef.asInstanceOf[ValDef[T]]
      def genericEmptyTree[T >: Untyped]: Thicket[T] = theEmptyTree.asInstanceOf[Thicket[T]]
      def genericEmptyModifiers[T >: Untyped]: Modifiers[T] = theEmptyModifiers.asInstanceOf[Modifiers[T]]

      trait WithLazyField[+T <: AnyRef]

      abstract class Instance[T >: Untyped <: Type] extends util.DotClass { inst =>

        type Modifiers = Trees.Modifiers[T]
        type Tree = Trees.Tree[T]
        type TypTree = Trees.TypTree[T]
        type TermTree = Trees.TermTree[T]
        type RefTree = Trees.RefTree[T]
        type MemberDef = Trees.MemberDef[T]
        type ValOrDefDef = Trees.ValOrDefDef[T]

        type Ident = Trees.Ident[T]
        type Select = Trees.Select[T]
        type This = Trees.This[T]
        type Super = Trees.Super[T]
        type Apply = Trees.Apply[T]
        type TypeApply = Trees.TypeApply[T]
        type Literal = Trees.Literal[T]
        type New = Trees.New[T]
        type Pair = Trees.Pair[T]
        type Typed = Trees.Typed[T]
        type NamedArg = Trees.NamedArg[T]
        type Assign = Trees.Assign[T]
        type Block = Trees.Block[T]
        type If = Trees.If[T]
        type Closure = Trees.Closure[T]
        type Match = Trees.Match[T]
        type CaseDef = Trees.CaseDef[T]
        type Return = Trees.Return[T]
        type Try = Trees.Try[T]
        type SeqLiteral = Trees.SeqLiteral[T]
        type JavaSeqLiteral = Trees.JavaSeqLiteral[T]
        type TypeTree = Trees.TypeTree[T]
        type SingletonTypeTree = Trees.SingletonTypeTree[T]
        type AppliedTypeTree = Trees.AppliedTypeTree[T]
        type ByNameTypeTree = Trees.ByNameTypeTree[T]
        type TypeBoundsTree = Trees.TypeBoundsTree[T]
        type Bind = Trees.Bind[T]
        type Alternative = Trees.Alternative[T]
        type UnApply = Trees.UnApply[T]
        type ValDef = Trees.ValDef[T]
        type DefDef = Trees.DefDef[T]
        type TypeDef = Trees.TypeDef[T]
        type Template = Trees.Template[T]
        type Import = Trees.Import[T]
        type PackageDef = Trees.PackageDef[T]
        type Annotated = Trees.Annotated[T]
        type Thicket = Trees.Thicket[T]

        val EmptyTree: Thicket = genericEmptyTree
        val EmptyValDef: ValDef = genericEmptyValDef
        val EmptyModifiers: Modifiers = genericEmptyModifiers
      }
    }
    
    object tpd extends Trees.Instance[Type] {      
      def Modifiers(sym: Symbol): Modifiers = ast.Trees.Modifiers(
        sym.flags & ModifierFlags,
        if (sym.privateWithin.exists) sym.privateWithin.asType.name else tpnme.EMPTY,
        sym.annotations map (_.tree))

      def Ident(name: Name)/*(tp: NamedType)*/: Ident =
        new Ident(name)

      def Select(qualifier: Tree, name: Name): Select =
        new Select(qualifier, name)

//      def SelectFromTypeTree(qualifier: Tree, name: Name): SelectFromTypeTree =
//        new SelectFromTypeTree(qualifier, name)

      def This(qual: TypeName)/*(cls: ClassSymbol)*/: This =
        new This(qual)

      def Super(qual: Tree, mix: TypeName, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol): Super =
        new Super(qual, mix)

      def Apply(fn: Tree, args: List[Tree]): Apply =
        new Apply(fn, args)

      def TypeApply(fn: Tree, args: List[Tree]): TypeApply =
        new TypeApply(fn, args)

      def Literal(const: Constant): Literal =
        new Literal(const)

      def unitLiteral: Literal =
        Literal(Constant(()))

      def New(tpt: Tree): New =
        new New(tpt)

      def New(tp: Type): New = New(TypeTree(tp))

      def Pair(left: Tree, right: Tree): Pair =
        new Pair(left, right)

      def Typed(expr: Tree, tpt: Tree): Typed =
        new Typed(expr, tpt)

      def NamedArg(name: Name, arg: Tree) =
        new NamedArg(name, arg)

      def Assign(lhs: Tree, rhs: Tree): Assign =
        new Assign(lhs, rhs)

      def Block(stats: List[Tree], expr: Tree): Block =
        new Block(stats, expr)

      def If(cond: Tree, thenp: Tree, elsep: Tree): If =
        new If(cond, thenp, elsep)

      def Closure(env: List[Tree], meth: Tree, tpt: Tree): Closure =
        new Closure(env, meth, tpt)

      /**
       * A function def
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
//      def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targs: List[Tree] = Nil, targetType: Type = NoType): Block = {
//        val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
//        val call =
//          if (targs.isEmpty) Ident(TermRef(NoPrefix, meth))
//          else TypeApply(Ident(TermRef(NoPrefix, meth)), targs)
//        Block(
//          DefDef(meth, rhsFn) :: Nil,
//          Closure(Nil, call, targetTpt))
//      }

      def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef =
        new CaseDef(pat, guard, body)

      def Match(selector: Tree, cases: List[CaseDef]): Match =
        new Match(selector, cases)

      def Return(expr: Tree, from: Tree): Return =
        new Return(expr, from)

      def Try(block: Tree, cases: List[CaseDef], finalizer: Tree): Try =
        new Try(block, cases, finalizer)

      def SeqLiteral(elems: List[Tree]): SeqLiteral =
        new SeqLiteral(elems)

//      def SeqLiteral(tpe: Type, elems: List[Tree]): SeqLiteral =
//        if (tpe derivesFrom defn.SeqClass) SeqLiteral(elems) else JavaSeqLiteral(elems)

      def JavaSeqLiteral(elems: List[Tree]): SeqLiteral =
        new JavaSeqLiteral(elems)

      def TypeTree(original: Tree): TypeTree =
        TypeTree(original.tpe, original)

      def TypeTree(tp: Type, original: Tree = EmptyTree): TypeTree =
        new TypeTree(original).withType(tp)

      def TypeTree() = new TypeTree(EmptyTree)  

      def SingletonTypeTree(ref: Tree): SingletonTypeTree =
        new SingletonTypeTree(ref)

      def AppliedTypeTree(tycon: Tree, args: List[Tree]): AppliedTypeTree =
        new AppliedTypeTree(tycon, args)

      def ByNameTypeTree(result: Tree): ByNameTypeTree =
        new ByNameTypeTree(result)

      def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree =
        new TypeBoundsTree(lo, hi)

      def Bind(sym: TermSymbol, body: Tree): Bind =
        new Bind(sym.name, body)

      def Bind(name: TermName, body: Tree): Bind =
        new Bind(name, body)

      def Alternative(trees: List[Tree]): Alternative =
        new Alternative(trees)

      def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree], proto: Type): UnApply =
        new UnApply(fun, implicits, patterns)

      def ValDef(sym: TermSymbol, rhs: Tree = EmptyTree): ValDef =
        new ValDef(sym.name, TypeTree(sym.info), rhs)
      
      def ValDef(name: TermName, tpt: TypeTree, rhs: Tree): ValDef = 
        new ValDef(name, tpt, rhs)

//      def SyntheticValDef(name: TermName, rhs: Tree): ValDef =
//        ValDef(ctx.newSymbol(ctx.owner, name, Synthetic, rhs.tpe.widen, coord = rhs.pos), rhs)

//      def DefDef(sym: TermSymbol, rhs: Tree = EmptyTree): DefDef =
//        DefDef(sym, Function.const(rhs) _)

//      def DefDef(sym: TermSymbol, rhsFn: List[List[Tree]] => Tree): DefDef =
//        polyDefDef(sym, Function.const(rhsFn))
      
      def DefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef = new DefDef(name, tparams, vparamss, tpt, rhs)

      def TypeDef(sym: TypeSymbol): TypeDef =
        new TypeDef(sym.name, TypeTree(sym.info))
      
      def TypeDef(name: TypeName, tpt: Tree /*TypeTree*/) =
        new TypeDef(name, tpt)

      def Template(constr: DefDef, parents: List[Tree], self: ValDef, body: List[Tree]): Template = new Template(constr, parents, self, body)

      def ClassDef(name: TypeName, impl: Tree) =
        new TypeDef(name, impl)
      
      def ClassDef(cls: ClassSymbol, constr: DefDef, body: List[Tree], superArgs: List[Tree] = Nil): TypeDef = ???

      /**
       * An anonymous class
       *
       *      new parents { forwarders }
       *
       */
//      def AnonClass(parents: List[Type], fns: List[TermSymbol], methNames: List[TermName]): Block = {
//        val owner = fns.head.owner
//        val parents1 =
//          if (parents.head.classSymbol.is(Trait)) defn.ObjectClass.typeRef :: parents
//          else parents
//        val cls = ctx.newNormalizedClassSymbol(owner, tpnme.ANON_FUN, Synthetic, parents1,
//          coord = fns.map(_.pos).reduceLeft(_ union _))
//        val constr = ctx.newConstructor(cls, Synthetic, Nil, Nil).entered
//        def forwarder(fn: TermSymbol, name: TermName) = {
//          val fwdMeth = fn.copy(cls, name, Synthetic | Method).entered.asTerm
//          DefDef(fwdMeth, prefss => ref(fn).appliedToArgss(prefss))
//        }
//        val forwarders = (fns, methNames).zipped.map(forwarder)
//        val cdef = ClassDef(cls, DefDef(constr), forwarders)
//        Block(cdef :: Nil, New(cls.typeRef, Nil))
//      }

      // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
//      def WhileDo(owner: Symbol, cond: Tree, body: List[Tree]): Tree = {
//        val sym = ctx.newSymbol(owner, nme.WHILE_PREFIX, Flags.Label | Flags.Synthetic,
//          MethodType(Nil, defn.UnitType), coord = cond.pos)
//
//        val call = Apply(ref(sym), Nil)
//        val rhs = If(cond, Block(body, call), unitLiteral)
//        Block(List(DefDef(sym, rhs)), call)
//      }

      def Import(expr: Tree, selectors: List[Tree]): Import =
        new Import(expr, selectors)

      def PackageDef(pid: RefTree, stats: List[Tree]): PackageDef =
        new PackageDef(pid, stats)

      def Annotated(annot: Tree, arg: Tree): Annotated =
        new Annotated(annot, arg)

//      def Throw(expr: Tree): Tree =
//        ref(defn.throwMethod).appliedTo(expr)
    }
  }
}
