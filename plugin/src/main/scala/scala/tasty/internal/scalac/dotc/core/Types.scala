package scala.tasty.internal.scalac.dotc
package core

//import util.common._
import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import Constants._
import Contexts._
import Annotations._
import Decorators._
import util.Positions.Position
//import util.{DotClass, SimpleMap}
import ast.tpd._
import printing.Texts._
import ast.untpd
import collection.{mutable, Seq, breakOut}
//import config.Config
import config.Printers._
import annotation.tailrec
import Flags.FlagSet
//import typer.Mode
import language.implicitConversions

object Types {

//  private var nextId = 0

  /** The class of types.
   *  The principal subclasses and sub-objects are as follows:
   *
   *  Type -+- ProxyType --+- NamedType ----+--- TypeRef
   *        |              |                 \
   *        |              +- SingletonType-+-+- TermRef
   *        |              |                |
   *        |              |                +--- ThisType
   *        |              |                +--- SuperType
   *        |              |                +--- ConstantType
   *        |              |                +--- MethodParam
   *        |              |                +----RefinedThis
   *        |              |                +--- SkolemType
   *        |              +- PolyParam
   *        |              +- RefinedType
   *        |              +- TypeBounds
   *        |              +- ExprType
   *        |              +- AnnotatedType
   *        |              +- TypeVar
   *        |
   *        +- GroundType -+- AndType
   *                       +- OrType
   *                       +- MethodType -----+- ImplicitMethodType
   *                       |                  +- JavaMethodType
   *                       +- PolyType
   *                       +- ClassInfo
   *                       |
   *                       +- NoType
   *                       +- NoPrefix
   *                       +- ErrorType
   *                       +- WildcardType
   */
  abstract class Type {

// ----- Tests -----------------------------------------------------

    /** Is this type different from NoType? */
    def exists: Boolean = true

    /** This type, if it exists, otherwise `that` type */
    def orElse(that: => Type) = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Is this an alias TypeBounds? */
    def isAlias: Boolean = this.isInstanceOf[TypeAlias]

    /** Map function `f` over elements of an AndType, rebuilding with function `g` */
    def mapReduceAnd[T](f: Type => T)(g: (T, T) => T)(implicit ctx: Context): T = stripTypeVar match {
      case AndType(tp1, tp2) => g(tp1.mapReduceAnd(f)(g), tp2.mapReduceAnd(f)(g))
      case _ => f(this)
    }

    /** Map function `f` over elements of an OrType, rebuilding with function `g` */
    final def mapReduceOr[T](f: Type => T)(g: (T, T) => T)(implicit ctx: Context): T = stripTypeVar match {
      case OrType(tp1, tp2) => g(tp1.mapReduceOr(f)(g), tp2.mapReduceOr(f)(g))
      case _ => f(this)
    }

// ----- Associated symbols ----------------------------------------------

    /** The type symbol associated with the type */
    final def typeSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
//    case ThisType(cls) => cls // needed?
      case tp: SingletonType => NoSymbol
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    /** The least (wrt <:<) set of class symbols of which this type is a subtype
     */
    final def classSymbols(implicit ctx: Context): List[ClassSymbol] = this match {
      case tp: ClassInfo =>
        tp.cls :: Nil
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym.asClass :: Nil else tp.underlying.classSymbols
      case tp: TypeProxy =>
        tp.underlying.classSymbols
      case AndType(l, r) =>
        l.classSymbols union r.classSymbols
      case OrType(l, r) =>
        l.classSymbols intersect r.classSymbols // TODO does not conform to spec
      case _ =>
        Nil
    }

    /** The term symbol associated with the type */
    final def termSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

// ----- Unwrapping types -----------------------------------------------

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not, until the result is no longer a TypeVar. Identity on all other types.
     */
    def stripTypeVar(implicit ctx: Context): Type = this

    /** Remove all AnnotatedTypes wrapping this type.
      */
    def stripAnnots(implicit ctx: Context): Type = this

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  Also go from => T to T.
     *  Identity for all other types. Example:
     *
     *  class Outer { class C ; val x: C }
     *  def o: Outer
     *  <o.x.type>.widen = o.C
     */
    final def widen(implicit ctx: Context): Type = widenSingleton match {
      case tp: ExprType => tp.resultType.widen
      case tp => tp
    }

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences.
     */
    final def widenSingleton(implicit ctx: Context): Type = stripTypeVar match {
      case tp: SingletonType if !tp.isOverloaded => tp.underlying.widenSingleton
      case _ => this
    }

    /** Widen from ExprType type to its result type.
     *  (Note: no stripTypeVar needed because TypeVar's can't refer to ExprTypes.)
     */
    final def widenExpr: Type = this match {
      case tp: ExprType => tp.resType
      case _ => this
    }

    /** If this is a TypeAlias type, its alias otherwise this type itself */
    final def followTypeAlias(implicit ctx: Context): Type = this match {
      case TypeAlias(alias) => alias
      case _ => this
    }

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(implicit ctx: Context): Type = stripTypeVar match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
    }

    /** If this is a refinement type, the unrefined parent,
     *  else the type itself.
     */
    final def unrefine(implicit ctx: Context): Type = stripTypeVar match {
      case tp @ RefinedType(tycon, _) => tycon.unrefine
      case _ => this
    }
    
    /** The chain of underlying types as long as type is a TypeProxy.
     *  Useful for diagnostics
     */
    def underlyingChain(implicit ctx: Context): List[Type] = this match {
      case tp: TypeProxy => tp :: tp.underlying.underlyingChain
      case _ => Nil
    }

    /** For a ClassInfo type, its parents,
     *  Inherited by all type proxies. Empty for all other types.
     *  Overwritten in ClassInfo, where parents is cached.
     */
    def parents(implicit ctx: Context): List[TypeRef] = this match {
      case tp: TypeProxy => tp.underlying.parents
      case _ => List()
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramTypess(implicit ctx: Context): List[List[Type]] = this match {
      case mt: MethodType => mt.paramTypes :: mt.resultType.paramTypess
      case pt: PolyType => pt.resultType.paramTypess
      case _ => Nil
    }

    /** The parameter types in the first parameter section of a PolyType or MethodType, Empty list for others */
    final def firstParamTypes(implicit ctx: Context): List[Type] = this match {
      case mt: MethodType => mt.paramTypes
      case pt: PolyType => pt.resultType.firstParamTypes
      case _ => Nil
    }

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless(implicit ctx: Context): Boolean = this match {
      case mt: MethodType => false
      case pt: PolyType => pt.resultType.isParameterless
      case _ => true
    }

    /** The resultType of a PolyType, MethodType, or ExprType, the type itself for others */
    def resultType(implicit ctx: Context): Type = this

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType(implicit ctx: Context): Type = resultType match {
      case mt: MethodType => mt.resultType.finalResultType
      case pt: PolyType => pt.resultType.finalResultType
      case _ => resultType
    }

    /** This type seen as a TypeBounds */
    final def bounds(implicit ctx: Context): TypeBounds = this match {
      case tp: TypeBounds => tp
      case ci: ClassInfo => TypeAlias(ci.typeRef)
      case wc: WildcardType =>
        wc.optBounds match {
          case bounds: TypeBounds => bounds
          case NoType => TypeBounds.empty
        }
      case _ => TypeAlias(this)
    }

    /** If this is a prototype with some ignored component, reveal one more
     *  layer of it. Otherwise the type itself.
     */
    def deepenProto(implicit ctx: Context): Type = this

// ----- Substitutions -----------------------------------------------------

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRefWithSignature types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(implicit ctx: Context): Signature = Signature.NotAMethod
  } // end Type

// ----- Type categories ----------------------------------------------

  /** A marker trait for cached types */
  trait CachedType extends Type

  /** A marker trait for type proxies.
   *  Each implementation is expected to redefine the `underlying` method.
   */
  abstract class TypeProxy extends Type {
    /** The type to which this proxy forwards operations. */
    def underlying(implicit ctx: Context): Type
  }

  // Every type has to inherit one of the following four abstract type classes.,
  // which determine whether the type is cached, and whether
  // it is a proxy of some other type. The duplication in their methods
  // is for efficiency.

  /**  Instances of this class are cached and are not proxies. */
  abstract class CachedGroundType extends Type with CachedType

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType

  /**  Instances of this class are uncached and are not proxies. */
  abstract class UncachedGroundType extends Type

  /**  Instances of this class are uncached and are proxies. */
  abstract class UncachedProxyType extends TypeProxy

  /** A marker trait for types that apply only to type symbols */
  trait TypeType extends Type

  /** A marker trait for types that apply only to term symbols */
  trait TermType extends Type

  /** A marker trait for types that can be types of values or prototypes of value types */
  trait ValueTypeOrProto extends TermType

  /** A marker trait for types that can be types of values */
  trait ValueType extends ValueTypeOrProto

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded(implicit ctx: Context) = false
  }

  /** A marker trait for types that bind other types that refer to them.
   *  Instances are: PolyType, MethodType, RefinedType.
   */
  trait BindingType extends Type

  /** A trait for proto-types, used as expected types in typer */
  trait ProtoType extends Type {
    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T
    def map(tm: TypeMap)(implicit ctx: Context): ProtoType
  }

  /** Implementations of this trait cache the results of `narrow`. */
  trait NarrowCached extends Type

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name */
  abstract class NamedType extends CachedProxyType with ValueType {

    val prefix: Type
    val name: Name

    type ThisType >: this.type <: NamedType

    assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    protected def sig: Signature = Signature.NotAMethod

    private def withSig(sig: Signature)(implicit ctx: Context): NamedType = ???

    def symbol(implicit ctx: Context): Symbol = ???

    def info(implicit ctx: Context): Type = ??? //denot.info

    def isType = isInstanceOf[TypeRef]
    def isTerm = isInstanceOf[TermRef]

    /** Create a NamedType of the same kind as this type, but with a new prefix.
     */
    protected def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType(prefix, name)

    /** Create a NamedType of the same kind as this type, but with a "inherited name".
     *  This is necessary to in situations like the following:
     *
     *    class B { def m: T1 }
     *    class C extends B { private def m: T2; ... C.m }
     *    object C extends C
     *    object X { ... C.m }
     *
     *  The two references of C.m in class C and object X refer to different
     *  definitions: The one in C refers to C#m whereas the one in X refers to B#m.
     *  But the type C.m must have only one denotation, so it can't refer to two
     *  members depending on context.
     *
     *  In situations like this, the reference in X would get the type
     *  `<C.m>.shadowed` to make clear that we mean the inherited member, not
     *  the private one.
     *
     *  Note: An alternative, possibly more robust scheme would be to give
     *  private members special names. A private definition would have a special
     *  name (say m' in the example above), but would be entered in its enclosing
     *  under both private and public names, so it could still be found by looking up
     *  the public name.
     */
    final def shadowed(implicit ctx: Context): NamedType =
      NamedType(prefix, name.shadowedName)

    override def equals(that: Any) = that match {
      case that: NamedType =>
        this.name == that.name &&
        this.prefix == that.prefix &&
        !that.isInstanceOf[TermRefWithSignature] &&
        !that.isInstanceOf[WithFixedSym]
      case _ =>
        false
    }
  }

  abstract case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType {

    type ThisType = TermRef

    //assert(name.toString != "<local Coder>")
    override def underlying(implicit ctx: Context): Type = ???

    override def signature(implicit ctx: Context): Signature = ???

    override def isOverloaded(implicit ctx: Context) = ???
  }

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {

    type ThisType = TypeRef

    override def underlying(implicit ctx: Context): Type = ???
  }

  final class TermRefWithSignature(prefix: Type, name: TermName, override val sig: Signature) extends TermRef(prefix, name) {
    assert(prefix ne NoPrefix)
    override def signature(implicit ctx: Context) = sig

    override def equals(that: Any) = that match {
      case that: TermRefWithSignature =>
        this.prefix == that.prefix &&
        this.name == that.name &&
        this.sig == that.sig
      case _ =>
        false
    }
  }

  trait WithFixedSym extends NamedType {
    def fixedSym: Symbol
    assert(fixedSym ne NoSymbol)

    override def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType.withFixedSym(prefix, fixedSym)

    override def equals(that: Any) = that match {
      case that: WithFixedSym => this.prefix == that.prefix && (this.fixedSym eq that.fixedSym)
      case _ => false
    }
  }

  final class CachedTermRef(prefix: Type, name: TermName, hc: Int) extends TermRef(prefix, name) {
    assert(prefix ne NoPrefix)
  }

  final class CachedTypeRef(prefix: Type, name: TypeName, hc: Int) extends TypeRef(prefix, name) {
    assert(prefix ne NoPrefix)
  }

  final class TermRefWithFixedSym(prefix: Type, name: TermName, val fixedSym: TermSymbol) extends TermRef(prefix, name) with WithFixedSym
  final class TypeRefWithFixedSym(prefix: Type, name: TypeName, val fixedSym: TypeSymbol) extends TypeRef(prefix, name) with WithFixedSym

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) = 
      if (name.isTermName) TermRef.all(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName) 
    def withFixedSym(prefix: Type, sym: Symbol)(implicit ctx: Context) =
      if (sym.isType) TypeRef.withFixedSym(prefix, sym.name.asTypeName, sym.asType)
      else TermRef.withFixedSym(prefix, sym.name.asTermName, sym.asTerm)
  }

  object TermRef {
    /** Create term ref with given name, without specifying a signature.
     *  Its meaning is the (potentially multi-) denotation of the member(s)
     *  of prefix with given name.
     */
    def all(prefix: Type, name: TermName)(implicit ctx: Context): TermRef = ???
    /** Create term ref referring to given symbol, taking the signature
     *  from the symbol if it is completed, or creating a term ref without
     *  signature, if symbol is not yet completed.
     */
    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef = ???

    /** Create a non-member term ref (which cannot be reloaded using `member`),
     *  with given prefix, name, and signature
     */
    def withFixedSym(prefix: Type, name: TermName, sym: TermSymbol)(implicit ctx: Context): TermRef = ???
  }

  object TypeRef {
    /** Create type ref with given prefix and name */
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context): TypeRef = ???
    /** Create type ref to given symbol */
    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRef = ???
    /** Create a non-member type ref  (which cannot be reloaded using `member`),
     *  with given prefix, name, and symbol.
     */
    def withFixedSym(prefix: Type, name: TypeName, sym: TypeSymbol)(implicit ctx: Context): TypeRef = ???

    /** Create a type ref referring to given symbol with given name.
     *  This is very similar to TypeRef(Type, Symbol),
     *  except for two differences:
     *  (1) The symbol might not yet have a denotation, so the name needs to be given explicitly.
     *  (2) The name in the type ref need not be the same as the name of the Symbol.
     */
    def withSymAndName(prefix: Type, sym: TypeSymbol, name: TypeName)(implicit ctx: Context): TypeRef = ???
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  /** The type cls.this
   *  @param tref    A type ref which indicates the class `cls`.
   *  Note: we do not pass a class symbol directly, because symbols
   *  do not survive runs whereas typerefs do.
   */
  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls(implicit ctx: Context): ClassSymbol = ???//tref.stableInRunSymbol.asClass
    override def underlying(implicit ctx: Context): Type = ???
  }

  final class CachedThisType(tref: TypeRef) extends ThisType(tref)

  object ThisType

  /** The type of a super reference cls.super where
   *  `thistpe` is cls.this and `supertpe` is the type of the value referenced
   *  by `super`.
   */
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = supertpe
    def derivedSuperType(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      if ((thistpe eq this.thistpe) && (supertpe eq this.supertpe)) this
      else SuperType(thistpe, supertpe)
  }

  final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context): Type = {
      assert(thistpe != NoPrefix)
      ???
    }
  }

  /** A constant type with  single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = value.tpe
  }

  final class CachedConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context) = ???
  }

  case class LazyRef(refFn: () => Type) extends UncachedProxyType with ValueType {
    lazy val ref = refFn()
    override def underlying(implicit ctx: Context) = ref
    override def toString = s"LazyRef($ref)"
    override def equals(other: Any) = other match {
      case other: LazyRef => this.ref.equals(other.ref)
      case _ => false
    }
    override def hashCode = ref.hashCode + 37
  }

  // --- Refined Type ---------------------------------------------------------

  /** A refined type parent { refinement }
   *  @param refinedName  The name of the refinement declaration
   *  @param infoFn: A function that produces the info of the refinement declaration,
   *                 given the refined type itself.
   */
  abstract case class RefinedType(parent: Type, refinedName: Name)
    extends CachedProxyType with BindingType with ValueType {

    val refinedInfo: Type

    private var refinementRefersToThisCache: Boolean = _
    private var refinementRefersToThisKnown: Boolean = false

    override def underlying(implicit ctx: Context) = parent

    override def equals(that: Any) = that match {
      case that: RefinedType =>
        this.parent == that.parent &&
        this.refinedName == that.refinedName &&
        this.refinedInfo == that.refinedInfo
      case _ =>
        false
    }
    override def toString = s"RefinedType($parent, $refinedName, $refinedInfo | $hashCode)" // !!! TODO: remove
  }

  class CachedRefinedType(parent: Type, refinedName: Name, infoFn: RefinedType => Type) extends RefinedType(parent, refinedName) {
    val refinedInfo = infoFn(this)
  }

  class PreHashedRefinedType(parent: Type, refinedName: Name, override val refinedInfo: Type, hc: Int)
  extends RefinedType(parent, refinedName)

  object RefinedType {
    def make(parent: Type, names: List[Name], infoFns: List[RefinedType => Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infoFns.head), names.tail, infoFns.tail)

    def apply(parent: Type, name: Name, infoFn: RefinedType => Type)(implicit ctx: Context): RefinedType = ???

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType = ???
  }

  // --- AndType/OrType ---------------------------------------------------------------

  trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
    def tp1: Type
    def tp2: Type
    def isAnd: Boolean
    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type  // needed?
  }

  abstract case class AndType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {

    def isAnd = true

    def derivedAndType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType.make(tp1, tp2)

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      derivedAndType(tp1, tp2)
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) = ???
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if (tp1 eq tp2) tp1 else apply(tp1, tp2)
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {
    assert(tp1.isInstanceOf[ValueType] && tp2.isInstanceOf[ValueType])
    def isAnd = false

    def derivedOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else OrType.make(tp1, tp2)

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      derivedOrType(tp1, tp2)
  }

  final class CachedOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) = ???
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if (tp1 eq tp2) tp1 else apply(tp1, tp2)
  }

  // ----- Method types: MethodType/ExprType/PolyType -------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  /** A trait that mixes in functionality for signature caching */
  trait MethodicType extends Type {
    final override def signature(implicit ctx: Context): Signature = ???
  }

  trait MethodOrPoly extends MethodicType

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>
    import MethodType._

    def isJava = false
    def isImplicit = false

    private[core] val resType = resultTypeExp(this)
    assert(resType.exists)

    override def resultType(implicit ctx: Context): Type = ???

    private def combine(x: DependencyStatus, y: DependencyStatus): DependencyStatus = {
      val status = (x & StatusMask) max (y & StatusMask)
      val provisional = (x | y) & Provisional
      (if (status == TrueDeps) status else status | provisional).toByte
    }

    override def equals(that: Any) = that match {
      case that: MethodType =>
        this.paramNames == that.paramNames &&
        this.paramTypes == that.paramTypes &&
        this.resType == that.resType
      case _ =>
        false
    }

    protected def prefixString = "MethodType"
    override def toString = s"$prefixString($paramNames, $paramTypes, $resType)"
  }

  final class CachedMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[CachedMethodType]
  }

  final class JavaMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isJava = true
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[JavaMethodType]
    override protected def prefixString = "JavaMethodType"
  }

  final class ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isImplicit = true
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[ImplicitMethodType]
    override protected def prefixString = "ImplicitMethodType"
  }

  abstract class MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType
    def apply(paramNames: List[TermName], paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(paramNames, paramTypes)(_ => resultType)
    def apply(paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      apply(nme.syntheticParamNames(paramTypes.length), paramTypes)(resultTypeExp)
    def apply(paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(nme.syntheticParamNames(paramTypes.length), paramTypes, resultType)
  }

  object MethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) = ???

    private type DependencyStatus = Byte
    private final val Unknown: DependencyStatus = 0   // not yet computed
    private final val NoDeps: DependencyStatus = 1    // no dependent parameters found
    private final val FalseDeps: DependencyStatus = 2 // all dependent parameters are prefixes of non-depended alias types
    private final val TrueDeps: DependencyStatus = 3  // some truly dependent parameters exist
    private final val StatusMask: DependencyStatus = 3 // the bits indicating actual dependency status
    private final val Provisional: DependencyStatus = 4  // set if dependency status can still change due to type variable instantiations
  }

  object JavaMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) = ???
  }

  object ImplicitMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) = ???
  }

  /** A by-name parameter type of the form `=> T`, or the type of a method with no parameter list. */
  abstract case class ExprType(resType: Type)
  extends CachedProxyType with TermType with MethodicType {
    override def resultType(implicit ctx: Context): Type = resType
    override def underlying(implicit ctx: Context): Type = resType
    def derivedExprType(resType: Type)(implicit ctx: Context) =
      if (resType eq this.resType) this else ExprType(resType)
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context) = {
      ???
    }
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly {

    val paramBounds = paramBoundsExp(this)
    val resType = resultTypeExp(this)

    override def resultType(implicit ctx: Context) = resType

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
//    override def computeHash = identityHash

    override def toString = s"PolyType($paramNames, $paramBounds, $resType)"
  }

  object PolyType

  // ----- Bound types: MethodParam, PolyParam, RefinedThis --------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    def binder: BT
    // Dotty deviation: copyBoundType was copy, but
    // dotty generates copy methods always automatically, and therefore
    // does not accept same-named method definitions in subclasses.
    // Scala2x, on the other hand, requires them (not sure why!)
    def copyBoundType(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
  }

  abstract case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType
    override def underlying(implicit ctx: Context): Type = binder.paramTypes(paramNum)
    def copyBoundType(bt: BT) = new MethodParamImpl(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam(${binder.paramNames(paramNum)})"
  }

  class MethodParamImpl(binder: MethodType, paramNum: Int) extends MethodParam(binder, paramNum)

  object MethodParam {
    def apply(binder: MethodType, paramNum: Int)(implicit ctx: Context): MethodParam = ???
  }

  /** TODO Some docs would be nice here! */
  case class PolyParam(binder: PolyType, paramNum: Int) extends ParamType {
    type BT = PolyType
    def copyBoundType(bt: BT) = PolyParam(bt, paramNum)

    /** Looking only at the structure of `bound`, is one of the following true?
     *     - fromBelow and param <:< bound
     *     - !fromBelow and param >:> bound
     */
    def occursIn(bound: Type, fromBelow: Boolean)(implicit ctx: Context): Boolean = bound.stripTypeVar match {
      case bound: PolyParam => bound == this
      case bound: AndOrType =>
        def occ1 = occursIn(bound.tp1, fromBelow)
        def occ2 = occursIn(bound.tp2, fromBelow)
        if (fromBelow == bound.isAnd) occ1 && occ2 else occ1 || occ2
      case _ => false
    }

    override def underlying(implicit ctx: Context): Type = binder.paramBounds(paramNum)
    // no customized hashCode/equals needed because cycle is broken in PolyType
    override def toString = s"PolyParam(${binder.paramNames(paramNum)})"

    override def equals(that: Any) = that match {
      case that: PolyParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }
  }

  /** a this-reference to an enclosing refined type `binder`. */
  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying(implicit ctx: Context) = binder
    def copyBoundType(bt: BT) = RefinedThis(bt)

    // need to customize hashCode and equals to prevent infinite recursion for
    // refinements that refer to the refinement type via this
    override def equals(that: Any) = that match {
      case that: RefinedThis => this.binder eq that.binder
      case _ => false
    }
    override def toString = s"RefinedThis(${binder.hashCode})"
  }

  // ----- Skolem types -----------------------------------------------

  /** A skolem type reference with underlying type `binder`. */
  abstract case class SkolemType(info: Type) extends CachedProxyType with ValueType with SingletonType {
    override def underlying(implicit ctx: Context) = info
    def derivedSkolemType(info: Type)(implicit ctx: Context) =
      if (info eq this.info) this else SkolemType(info)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    override def toString = s"Skolem($info)"
  }

  final class CachedSkolemType(info: Type) extends SkolemType(info)

  object SkolemType {
    def apply(info: Type)(implicit ctx: Context) = ???
  }

  // ------------ Type variables ----------------------------------------

  /** In a TypeApply tree, a TypeVar is created for each argument type to be inferred.
   *  Every type variable is referred to by exactly one inferred type parameter of some
   *  TypeApply tree.
   *
   *  A type variable is essentially a switch that models some part of a substitution.
   *  It is first linked to `origin`, a poly param that's in the current constraint set.
   *  It can then be (once) instantiated to some other type. The instantiation is
   *  recorded in the type variable itself, or else, if the current type state
   *  is different from the variable's creation state (meaning unrolls are possible)
   *  in the current typer state.
   *
   *  @param  origin        The parameter that's tracked by the type variable.
   *  @param  creatorState  The typer state in which the variable was created.
   *  @param  owningTree    The function part of the TypeApply tree tree that introduces
   *                        the type variable.
   *  @paran  owner         The current owner if the context where the variable was created.
   *
   *  `owningTree` and `owner` are used to determine whether a type-variable can be instantiated
   *  at some given point. See `Inferencing#interpolateUndetVars`.
   */
  final class TypeVar(val origin: PolyParam/*, creatorState: TyperState*/, val owningTree: untpd.Tree, val owner: Symbol) extends CachedProxyType with ValueType {
    /** Unwrap to instance (if instantiated) or origin (if not), until result
     *  is no longer a TypeVar
     */
    override def stripTypeVar(implicit ctx: Context): Type = ???

    /** If the variable is instantiated, its instance, otherwise its origin */
    override def underlying(implicit ctx: Context): Type = ???

//    override def computeHash: Int = identityHash
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def toString = ???
  }

  // ------ ClassInfo, Type Bounds ------------------------------------------------------------

  /** Roughly: the info of a class during a period.
   *  @param prefix       The prefix on which parents, decls, and selfType need to be rebased.
   *  @param cls          The class symbol.
   *  @param classParents The parent types of this class.
   *                      These are all normalized to be TypeRefs by moving any refinements
   *                      to be member definitions of the class itself.
   *  @param decls        The symbols defined directly in this class.
   *  @param selfInfo     The type of `this` in this class, if explicitly given,
   *                      NoType otherwise. If class is compiled from source, can also
   *                      be a reference to the self symbol containing the type.
   */
  abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      classParents: List[TypeRef],
      decls: Iterable[Symbol]/*Scope*/,
      //TODO add DotClass (should be for TreePickling)
      selfInfo: Type /* should be: Type | Symbol */) extends CachedGroundType with TypeType {

    /** The self type of a class is the conjunction of
     *   - the explicit self type if given (or the info of a given self symbol), and
     *   - the fully applied reference to the class itself.
     */
    def selfType(implicit ctx: Context): Type = ???

    private var selfTypeCache: Type = null

    def typeRef(implicit ctx: Context): Type = ???

    // cached because baseType needs parents
    private var parentsCache: List[TypeRef] = null

    /** The parent type refs as seen from the given prefix */
    override def parents(implicit ctx: Context): List[TypeRef] = ???

    def derivedClassInfo(prefix: Type)(implicit ctx: Context) = ???

    override def toString = s"ClassInfo($prefix, $cls)"
  }

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Iterable[Symbol] /*Scope*/, selfInfo: Type/*DotClass*/ = NoType)(implicit ctx: Context) = ???
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying(implicit ctx: Context): Type = hi

    /** The non-alias type bounds type with given bounds */
    def derivedTypeBounds(lo: Type, hi: Type)(implicit ctx: Context) =
      if ((lo eq this.lo) && (hi eq this.hi) && (variance == 0)) this
      else TypeBounds(lo, hi)

    /** If this is an alias, a derived alias with the new variance,
     *  Otherwise the type itself.
     */
    def withVariance(variance: Int)(implicit ctx: Context) = this match {
      case tp: TypeAlias => tp.derivedTypeAlias(tp.alias, variance)
      case _ => this
    }

    /** If this type and that type have the same variance, this variance, otherwise 0 */
    final def commonVariance(that: TypeBounds): Int = (this.variance + that.variance) / 2

    override def equals(that: Any): Boolean = that match {
      case that: TypeBounds =>
        (this.lo eq that.lo) && (this.hi eq that.hi) && this.variance == that.variance
      case _ =>
        false
    }

    override def toString =
      if (lo eq hi) s"TypeAlias($lo)" else s"TypeBounds($lo, $hi)"
  }

  class RealTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias) {
    /** pre: this is a type alias */
    def derivedTypeAlias(tp: Type, variance: Int = this.variance)(implicit ctx: Context) =
      if ((lo eq tp) && (variance == this.variance)) this
      else TypeAlias(tp, variance)
  }

  class CachedTypeAlias(alias: Type, variance: Int, hc: Int) extends TypeAlias(alias, variance)

  object TypeBounds {
    def apply(lo: Type, hi: Type)(implicit ctx: Context): TypeBounds = ???
    def empty(implicit ctx: Context) = ???
    def upper(hi: Type)(implicit ctx: Context) = ???
    def lower(lo: Type)(implicit ctx: Context) = ???
  }

  object TypeAlias {
    def apply(alias: Type, variance: Int = 0)(implicit ctx: Context) = ???
    def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  case class AnnotatedType(annot: Annotation, tpe: Type)
      extends UncachedProxyType with ValueType {
    // todo: cache them? but this makes only sense if annotations and trees are also cached.
    override def underlying(implicit ctx: Context): Type = tpe
    def derivedAnnotatedType(annot: Annotation, tpe: Type) =
      if ((annot eq this.annot) && (tpe eq this.tpe)) this
      else AnnotatedType(annot, tpe)

    override def stripTypeVar(implicit ctx: Context): Type =
      derivedAnnotatedType(annot, tpe.stripTypeVar)
    override def stripAnnots(implicit ctx: Context): Type = tpe.stripAnnots
  }

  object AnnotatedType {
    def make(annots: List[Annotation], underlying: Type) =
      if (annots.isEmpty) underlying
      else (underlying /: annots)((tp, ann) => AnnotatedType(ann, tp))
  }

  // Special type objects and classes -----------------------------------------------------

  /** The type of an erased array */
  abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType {
    def derivedJavaArrayType(elemtp: Type)(implicit ctx: Context) =
      if (elemtp eq this.elemType) this else JavaArrayType(elemtp)
  }
  final class CachedJavaArrayType(elemType: Type) extends JavaArrayType(elemType)
  object JavaArrayType {
    def apply(elemType: Type)(implicit ctx: Context) = ???
  }

  /** The type of an import clause tree */
  case class ImportType(expr: Tree) extends UncachedGroundType

  /** Sentinel for "missing type" */
  case object NoType extends CachedGroundType {
    override def exists = false
  }

  /** Missing prefix */
  case object NoPrefix extends CachedGroundType

  abstract class ErrorType extends UncachedGroundType with ValueType

  object ErrorType extends ErrorType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType {
    def derivedWildcardType(optBounds: Type)(implicit ctx: Context) =
      if (optBounds eq this.optBounds) this else WildcardType(optBounds.asInstanceOf[TypeBounds])
  }

  final class CachedWildcardType(optBounds: Type) extends WildcardType(optBounds)

  object WildcardType extends WildcardType(NoType) {
    def apply(bounds: TypeBounds)(implicit ctx: Context) = ???
  }

  // ----- TypeMaps --------------------------------------------------------------------

  abstract class TypeMap(implicit protected val ctx: Context) extends (Type => Type) { thisMap =>

    protected def stopAtStatic = true

    def apply(tp: Type): Type

    protected var variance = 1

    /** Can be overridden. By default, only the prefix is mapped. */
    protected def mapClassInfo(tp: ClassInfo): ClassInfo =
      tp.derivedClassInfo(this(tp.prefix))

    def andThen(f: Type => Type): TypeMap = new TypeMap {
      override def stopAtStatic = thisMap.stopAtStatic
      def apply(tp: Type) = f(thisMap(tp))
    }
  }

  /** A type map that maps also parents and self type of a ClassInfo */
  abstract class DeepTypeMap(implicit ctx: Context) extends TypeMap {
    override def mapClassInfo(tp: ClassInfo) = ???
  }

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T](implicit protected val ctx: Context) extends ((T, Type) => T) {

    protected def stopAtStatic = true

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected var variance = 1

    protected def applyToPrefix(x: T, tp: NamedType) = {
      val saved = variance
      variance = 0
      val result = this(x, tp.prefix)
      variance = saved
      result
    }

    final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  //   ----- Name Filters --------------------------------------------------

  /** A name filter selects or discards a member name of a type `pre`.
   *  To enable efficient caching, name filters have to satisfy the
   *  following invariant: If `keep` is a name filter, and `pre` has
   *  class `C` as a base class, then
   *
   *    keep(pre, name)  implies  keep(C.this, name)
   */
  abstract class NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean
  }

  object typeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = name.isTypeName
  }

  object takeAllFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = true
  }

  object implicitFilter extends NameFilter {
    /** A dummy filter method.
     *  Implicit filtering is handled specially in computeMemberNames, so
     *  no post-filtering is needed.
     */
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = true
  }

  // ----- Exceptions -------------------------------------------------------------

  class TypeError(msg: String) extends Exception(msg)
  class FatalTypeError(msg: String) extends TypeError(msg)

  class MergeError(msg: String) extends FatalTypeError(msg)

  // ----- Debug ---------------------------------------------------------

  var debugTrace = false

  val watchList = List[String](
  ) map (_.toTypeName)

  def isWatched(tp: Type) = tp match {
    case TypeRef(_, name) => watchList contains name
    case _ => false
  }

  // ----- Decorator implicits --------------------------------------------

  //*
  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
}
