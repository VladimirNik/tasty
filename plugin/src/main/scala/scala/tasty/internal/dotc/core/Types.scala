package scala.tasty.internal.dotc
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
import collection.{mutable, Seq, breakOut}
import config.Printers._
import annotation.tailrec
import Flags.FlagSet
import language.implicitConversions

object Types {
  abstract class Type extends DotClass {
    def exists: Boolean = true

    def orElse(that: => Type) = if (exists) this else that

    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    def isAlias: Boolean = this.isInstanceOf[TypeAlias]

    final def typeSymbol: Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
//    case ThisType(cls) => cls // needed?
      case tp: SingletonType => NoSymbol
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    def stripTypeVar: Type = this

    final def isParameterless: Boolean = this match {
      case mt: MethodType => false
      case pt: PolyType => pt.resultType.isParameterless
      case _ => true
    }

    def resultType: Type = this

    final def bounds: TypeBounds = this match {
      case tp: TypeBounds => tp
      case ci: ClassInfo => TypeAlias(ci.typeRef)
      case wc: WildcardType =>
        wc.optBounds match {
          case bounds: TypeBounds => bounds
          case NoType => TypeBounds.empty
        }
      case _ => TypeAlias(this)
    }

    def signature: Signature = Signature.NotAMethod
  } // end Type

  trait CachedType extends Type

  abstract class TypeProxy extends Type {
    def underlying: Type
  }

  abstract class CachedGroundType extends Type with CachedType

  abstract class CachedProxyType extends TypeProxy with CachedType

  abstract class UncachedGroundType extends Type

  abstract class UncachedProxyType extends TypeProxy

  trait TypeType extends Type

  trait TermType extends Type

  trait ValueTypeOrProto extends TermType

  trait ValueType extends ValueTypeOrProto

  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded = false
  }

  trait BindingType extends Type

  trait NarrowCached extends Type

  abstract class NamedType extends CachedProxyType with ValueType {
    val prefix: Type
    val name: Name

    type ThisType >: this.type <: NamedType

    //assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    protected def sig: Signature = Signature.NotAMethod

    def symbol: Symbol = ???

    def info: Type = ??? //denot.info

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

    override def underlying: Type = ???
  }

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {
    type ThisType = TypeRef

    override def underlying: Type = ???
  }

  final class TermRefWithSignature(prefix: Type, name: TermName, override val sig: Signature) extends TermRef(prefix, name) {
    assert(prefix ne NoPrefix)
    override def signature = sig

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
    def apply(prefix: Type, sym: TermSymbol): TermRef = ???
  }

  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls: ClassSymbol = ???//tref.stableInRunSymbol.asClass
    override def underlying: Type = ???
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying = supertpe
  }

  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying = value.tpe
  }

  case class LazyRef(refFn: () => Type) extends UncachedProxyType with ValueType {
    lazy val ref = refFn()
    override def underlying = ref
    override def toString = s"LazyRef($ref)"
    override def equals(other: Any) = other match {
      case other: LazyRef => this.ref.equals(other.ref)
      case _ => false
    }
    override def hashCode = ref.hashCode + 37
  }

  abstract case class RefinedType(parent: Type, refinedName: Name)
    extends CachedProxyType with BindingType with ValueType {

    val refinedInfo: Type

    override def underlying = parent

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

  trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
    def tp1: Type
    def tp2: Type
    def isAnd: Boolean
  }

  trait MethodicType extends Type {
    final override def signature: Signature = ???
  }

  trait MethodOrPoly extends MethodicType

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>
    import MethodType._

    private[core] val resType = resultTypeExp(this)
    assert(resType.exists)

    override def resultType: Type = ???

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

  abstract case class ExprType(resType: Type)
  extends CachedProxyType with TermType with MethodicType {
    override def resultType: Type = resType
    override def underlying: Type = resType
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly {

    val paramBounds = paramBoundsExp(this)
    val resType = resultTypeExp(this)

    override def resultType = resType

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]

    override def toString = s"PolyType($paramNames, $paramBounds, $resType)"
  }

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    def binder: BT
    // Dotty deviation: copyBoundType was copy, but
    // dotty generates copy methods always automatically, and therefore
    // does not accept same-named method definitions in subclasses.
    // Scala2x, on the other hand, requires them (not sure why!)
    //def copyBoundType(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
  }

  abstract case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType
    override def underlying: Type = binder.paramTypes(paramNum)
    //def copyBoundType(bt: BT) = new MethodParamImpl(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam(${binder.paramNames(paramNum)})"
  }

  case class PolyParam(binder: PolyType, paramNum: Int) extends ParamType {
    type BT = PolyType
    //def copyBoundType(bt: BT) = PolyParam(bt, paramNum)

    override def underlying: Type = binder.paramBounds(paramNum)
    // no customized hashCode/equals needed because cycle is broken in PolyType
    override def toString = s"PolyParam(${binder.paramNames(paramNum)})"

    override def equals(that: Any) = that match {
      case that: PolyParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }
  }

  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying = binder
    //def copyBoundType(bt: BT) = RefinedThis(bt)

    // need to customize hashCode and equals to prevent infinite recursion for
    // refinements that refer to the refinement type via this
    override def equals(that: Any) = that match {
      case that: RefinedThis => this.binder eq that.binder
      case _ => false
    }
    override def toString = s"RefinedThis(${binder.hashCode})"
  }

  abstract case class SkolemType(info: Type) extends CachedProxyType with ValueType with SingletonType {
    override def underlying = info
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    override def toString = s"Skolem($info)"
  }

  final class TypeVar(val origin: PolyParam/*, creatorState: TyperState, val owningTree: untpd.Tree*/, val owner: Symbol) extends CachedProxyType with ValueType {
    override def stripTypeVar: Type = ???

    override def underlying: Type = ???

    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def toString = ???
  }

  abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      classParents: List[TypeRef],
      decls: Iterable[Symbol]/*Scope*/,
      selfInfo: DotClass /* should be: Type | Symbol */) extends CachedGroundType with TypeType {

    def typeRef: Type = ???

    override def toString = s"ClassInfo($prefix, $cls)"
  }

  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying: Type = hi

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
    def empty = ???
  }

  object TypeAlias {
    def apply(alias: Type, variance: Int = 0) = ???
  }

  case class AnnotatedType(annot: Annotation, tpe: Type)
      extends UncachedProxyType with ValueType {
    override def underlying: Type = tpe

    override def stripTypeVar: Type = ???
  }

  case object NoType extends CachedGroundType {
    override def exists = false
  }

  case object NoPrefix extends CachedGroundType

  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType

  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
}
