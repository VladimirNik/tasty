package scala.tasty.internal.scalac.dotc
package core

import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import Constants._
import Contexts._
import Annotations._
import Decorators._
import util.Positions.Position
import util.DotClass
import ast.tpd._
import printing.Texts._
import ast.untpd
import collection.{mutable, Seq, breakOut}
import config.Printers._
import annotation.tailrec
import Flags.FlagSet
import language.implicitConversions

object Types {

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
  abstract class Type extends DotClass {

// ----- Tests -----------------------------------------------------

    /** Is this type different from NoType? */
    def exists: Boolean = true

    /** This type, if it exists, otherwise `that` type */
    def orElse(that: => Type) = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Is this an alias TypeBounds? */
    def isAlias: Boolean = this.isInstanceOf[TypeAlias]

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

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not, until the result is no longer a TypeVar. Identity on all other types.
     */
    def stripTypeVar(implicit ctx: Context): Type = this

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless(implicit ctx: Context): Boolean = this match {
      case mt: MethodType => false
      case pt: PolyType => pt.resultType.isParameterless
      case _ => true
    }

    /** The resultType of a PolyType, MethodType, or ExprType, the type itself for others */
    def resultType(implicit ctx: Context): Type = this

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

  /** Implementations of this trait cache the results of `narrow`. */
  trait NarrowCached extends Type

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name */
  abstract class NamedType extends CachedProxyType with ValueType {

    val prefix: Type
    val name: Name

    type ThisType >: this.type <: NamedType

    //assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    protected def sig: Signature = Signature.NotAMethod

    def symbol(implicit ctx: Context): Symbol = ???

    def info(implicit ctx: Context): Type = ??? //denot.info

    def isType = isInstanceOf[TypeRef]
    def isTerm = isInstanceOf[TermRef]

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

    override def underlying(implicit ctx: Context): Type = ???
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
    override def equals(that: Any) = ???
  }

  object TermRef {
    /** Create term ref referring to given symbol, taking the signature
     *  from the symbol if it is completed, or creating a term ref without
     *  signature, if symbol is not yet completed.
     */
    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef = ???
  }

  /** The type cls.this
   *  @param tref    A type ref which indicates the class `cls`.
   *  Note: we do not pass a class symbol directly, because symbols
   *  do not survive runs whereas typerefs do.
   */
  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls(implicit ctx: Context): ClassSymbol = ???//tref.stableInRunSymbol.asClass
    override def underlying(implicit ctx: Context): Type = ???
  }

  /** The type of a super reference cls.super where
   *  `thistpe` is cls.this and `supertpe` is the type of the value referenced
   *  by `super`.
   */
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = supertpe
  }

  /** A constant type with  single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = value.tpe
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

  // --- AndType/OrType ---------------------------------------------------------------

  trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
    def tp1: Type
    def tp2: Type
    def isAnd: Boolean
  }

  // ----- Method types: MethodType/ExprType/PolyType -------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

//  /** A trait that mixes in functionality for signature caching */
  trait MethodicType extends Type {
    final override def signature(implicit ctx: Context): Signature = ???
  }

  trait MethodOrPoly extends MethodicType

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>
    import MethodType._

    private[core] val resType = resultTypeExp(this)
    assert(resType.exists)

    override def resultType(implicit ctx: Context): Type = ???

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

  /** A by-name parameter type of the form `=> T`, or the type of a method with no parameter list. */
  abstract case class ExprType(resType: Type)
  extends CachedProxyType with TermType with MethodicType {
    override def resultType(implicit ctx: Context): Type = resType
    override def underlying(implicit ctx: Context): Type = resType
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly {

    val paramBounds = paramBoundsExp(this)
    val resType = resultTypeExp(this)

    override def resultType(implicit ctx: Context) = resType

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]

    override def toString = s"PolyType($paramNames, $paramBounds, $resType)"
  }

  // ----- Bound types: MethodParam, PolyParam, RefinedThis --------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    def binder: BT
    // Dotty deviation: copyBoundType was copy, but
    // dotty generates copy methods always automatically, and therefore
    // does not accept same-named method definitions in subclasses.
    // Scala2x, on the other hand, requires them (not sure why!)
//    def copyBoundType(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
  }

  abstract case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType
    override def underlying(implicit ctx: Context): Type = binder.paramTypes(paramNum)
//    def copyBoundType(bt: BT) = new MethodParamImpl(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam(${binder.paramNames(paramNum)})"
  }

  /** TODO Some docs would be nice here! */
  case class PolyParam(binder: PolyType, paramNum: Int) extends ParamType {
    type BT = PolyType
//    def copyBoundType(bt: BT) = PolyParam(bt, paramNum)

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
//    def copyBoundType(bt: BT) = RefinedThis(bt)

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
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    override def toString = s"Skolem($info)"
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
      selfInfo: DotClass /* should be: Type | Symbol */) extends CachedGroundType with TypeType {

    def typeRef(implicit ctx: Context): Type = ???

    override def toString = s"ClassInfo($prefix, $cls)"
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying(implicit ctx: Context): Type = hi

    override def equals(that: Any): Boolean = that match {
      case that: TypeBounds =>
        (this.lo eq that.lo) && (this.hi eq that.hi) && this.variance == that.variance
      case _ =>
        false
    }

    override def toString =
      if (lo eq hi) s"TypeAlias($lo)" else s"TypeBounds($lo, $hi)"
  }

  abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias)
  
  object TypeBounds {
    def empty(implicit ctx: Context) = ???
  }

  object TypeAlias {
    def apply(alias: Type, variance: Int = 0)(implicit ctx: Context) = ???
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  case class AnnotatedType(annot: Annotation, tpe: Type)
      extends UncachedProxyType with ValueType {
    // todo: cache them? but this makes only sense if annotations and trees are also cached.
    override def underlying(implicit ctx: Context): Type = tpe

    override def stripTypeVar(implicit ctx: Context): Type = ???
  }

  // Special type objects and classes -----------------------------------------------------

  /** Sentinel for "missing type" */
  case object NoType extends CachedGroundType {
    override def exists = false
  }

  /** Missing prefix */
  case object NoPrefix extends CachedGroundType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType

  // ----- Decorator implicits --------------------------------------------
  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
}
