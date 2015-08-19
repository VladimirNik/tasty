package scala.tasty.internal
package dotc
package core

trait TTypes {
  self: API =>

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
  import collection.{ mutable, Seq, breakOut }
  import config.Printers._
  import annotation.tailrec
  import Flags.FlagSet
  import language.implicitConversions
  import Denotations._
  import SymDenotations.SymDenotation

  object Types {
    abstract class Type extends DotClass {
      def exists: Boolean = true

      def orElse(that: => Type) = if (exists) this else that

      final def isValueType: Boolean = this.isInstanceOf[ValueType]

      def isAlias: Boolean = this.isInstanceOf[TypeAlias]

      final def typeSymbol: Symbol = this match {
        case tp: TypeRef       => tp.symbol
        case tp: ClassInfo     => tp.cls
        //    case ThisType(cls) => cls // needed?
        case tp: SingletonType => NoSymbol
        case tp: TypeProxy     => tp.underlying.typeSymbol
        case _                 => NoSymbol
      }

      def stripTypeVar: Type = this

      def stripAnnots: Type = this

      final def isParameterless: Boolean = this match {
        case mt: MethodType => false
        case pt: PolyType   => pt.resultType.isParameterless
        case _              => true
      }

      def resultType: Type = this

      final def bounds: TypeBounds = this match {
        case tp: TypeBounds => tp
        case ci: ClassInfo  => TypeAlias(ci.typeRef)
        case wc: WildcardType =>
          wc.optBounds match {
            case bounds: TypeBounds => bounds
            case NoType             => TypeBounds.empty
          }
        case _ => TypeAlias(this)
      }

      def signature: Signature = Signature.NotAMethod

      protected[TTypes] var initGType: g.Type = g.NoType
      def withGType(gType: g.Type): Type = {
        initGType = gType
        this
      }
      def initType = initGType
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

      def symbol: Symbol = denot.symbol

      def info: Type = denot.info

//      private def denotOfSym(sym: Symbol): Denotation = {
//        val d = sym.denot
//        val owner = d.owner
//        if (owner.isTerm) d else d.asSeenFrom(prefix)
//      }

      private[this] var myDenot: Denotation = _

      final def denot: Denotation = myDenot

      def withDenot(denot: Denotation): ThisType =
        if (sig != denot.signature)
          withSig(denot.signature).withDenot(denot).asInstanceOf[ThisType]
        else {
          setDenot(denot)
          this
        }

      final def setDenot(denot: Denotation): Unit = {
        myDenot = denot
      }

      def withSym(sym: Symbol, signature: Signature): ThisType =
        if (sig != signature)
          withSig(signature).withSym(sym, signature).asInstanceOf[ThisType]
        else {
          setSym(sym)
          this
        }

      def setSym(sym: Symbol): Unit = {
        myDenot = sym.denot
      }

      private def withSig(sig: Signature): NamedType =
        TermRef.withSig(prefix, name.asTermName, sig)

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
      override def signature: Signature = denot.signature
      override def underlying: Type = {
        val d = denot
        //denot isOverloaded = isInstanceOf[MultiDenotation]
        //if (d.isOverloaded) NoType else d.info
        d.info
      }

    }

    abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {
      type ThisType = TypeRef

      override def underlying: Type = info
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
      def fixedSym: Symbol
      withDenot(fixedSym)

      override def withSym(sym: Symbol, signature: Signature): ThisType =
        unsupported("withSym")

      override def withDenot(denot: Denotation): ThisType = {
        setDenot(denot)
        this
      }

      override def equals(that: Any) = that match {
        case that: WithFixedSym => this.prefix == that.prefix && (this.fixedSym eq that.fixedSym)
        case _                  => false
      }
    }

    final class CachedTermRef(prefix: Type, name: TermName /*, hc: Int */) extends TermRef(prefix, name) {
      assert(prefix ne NoPrefix)
    }

    final class CachedTypeRef(prefix: Type, name: TypeName /*, hc: Int */) extends TypeRef(prefix, name) {
      assert(prefix ne NoPrefix)
    }

    final class TermRefWithFixedSym(prefix: Type, name: TermName, val fixedSym: TermSymbol) extends TermRef(prefix, name) with WithFixedSym
    final class TypeRefWithFixedSym(prefix: Type, name: TypeName, val fixedSym: TypeSymbol) extends TypeRef(prefix, name) with WithFixedSym

    object NamedType {
      def apply(prefix: Type, name: Name) =
        if (name.isTermName) TermRef.all(prefix, name.asTermName)
        else TypeRef(prefix, name.asTypeName)
      def apply(prefix: Type, name: Name, denot: Denotation) =
        if (name.isTermName) TermRef(prefix, name.asTermName, denot)
        else TypeRef(prefix, name.asTypeName, denot)
      def withFixedSym(prefix: Type, sym: Symbol) =
        if (sym.isType) TypeRef.withFixedSym(prefix, sym.name.asTypeName, sym.asType)
        else TermRef.withFixedSym(prefix, sym.name.asTermName, sym.asTerm)
      def withSymAndName(prefix: Type, sym: Symbol, name: Name): NamedType =
        if (sym.isType) TypeRef.withSymAndName(prefix, sym.asType, name.asTypeName)
        else TermRef.withSymAndName(prefix, sym.asTerm, name.asTermName)
    }

    object TermRef {
      //phase is after resolve super
      private def symbolicRefs = false

      def all(prefix: Type, name: TermName): TermRef = {
        new CachedTermRef(prefix, name)
      }

      def apply(prefix: Type, sym: TermSymbol): TermRef =
        withSymAndName(prefix, sym, sym.name)

      def apply(prefix: Type, name: TermName, denot: Denotation): TermRef = {
        if ((prefix eq NoPrefix) || symbolicRefs)
          apply(prefix, denot.symbol.asTerm)
        else denot match {
          case denot: SymDenotation /* if denot.isCompleted */ => withSig(prefix, name, denot.signature)
          case _                                         => all(prefix, name)
        }
      } withDenot denot

      def withFixedSym(prefix: Type, name: TermName, sym: TermSymbol): TermRef =
        new TermRefWithFixedSym(prefix, name, sym)

      def withSymAndName(prefix: Type, sym: TermSymbol, name: TermName): TermRef =
        if ((prefix eq NoPrefix) || symbolicRefs)
          withFixedSym(prefix, name, sym)
        else //if (sym.isCompleted)
          withSig(prefix, name, sym.signature) withSym (sym, sym.signature)
//        else
//          all(prefix, name) withSym (sym, Signature.NotAMethod)

      def withSig(prefix: Type, sym: TermSymbol): TermRef =
        if ((prefix eq NoPrefix) || symbolicRefs) withFixedSym(prefix, sym.name, sym)
        else withSig(prefix, sym.name, sym.signature).withSym(sym, sym.signature)

      def withSig(prefix: Type, name: TermName, sig: Signature): TermRef =
        new TermRefWithSignature(prefix, name, sig)

      def withSigAndDenot(prefix: Type, name: TermName, sig: Signature, denot: Denotation): TermRef = {
        if ((prefix eq NoPrefix) || symbolicRefs)
          withFixedSym(prefix, denot.symbol.asTerm.name, denot.symbol.asTerm)
        else
          withSig(prefix, name, sig)
      } withDenot denot
    }

    object TypeRef {
      def apply(prefix: Type, name: TypeName): TypeRef =
        new CachedTypeRef(prefix, name)

      def apply(prefix: Type, sym: TypeSymbol): TypeRef =
        withSymAndName(prefix, sym, sym.name)

      def withFixedSym(prefix: Type, name: TypeName, sym: TypeSymbol): TypeRef =
        new TypeRefWithFixedSym(prefix, name, sym)

      def withSymAndName(prefix: Type, sym: TypeSymbol, name: TypeName): TypeRef =
        if (prefix eq NoPrefix) withFixedSym(prefix, name, sym)
        else apply(prefix, name).withSym(sym, Signature.NotAMethod)

      def apply(prefix: Type, name: TypeName, denot: Denotation): TypeRef = {
        if (prefix eq NoPrefix) apply(prefix, denot.symbol.asType)
        else apply(prefix, name)
      } withDenot denot
    }

    abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
      def cls: ClassSymbol = tref.symbol.asClass
      override def underlying: Type = ???
    }

    final class CachedThisType(tref: TypeRef) extends ThisType(tref)

    object ThisType {
      /** Normally one should use ClassSymbol#thisType instead */
      def raw(tref: TypeRef) =
        new CachedThisType(tref)
    }

    abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
      override def underlying = supertpe
    }

    abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
      override def underlying = value.tpe
    }

    final class CachedConstantType(value: Constant) extends ConstantType(value)

    final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

    object SuperType {
      def apply(thistpe: Type, supertpe: Type): Type = {
        assert(thistpe != NoPrefix)
        new CachedSuperType(thistpe, supertpe)
      }
    }

    object ConstantType {
      def apply(value: Constant) = {
        new CachedConstantType(value)
      }
    }

    case class LazyRef(refFn: () => Type) extends UncachedProxyType with ValueType {
      lazy val ref = refFn()
      override def underlying = ref
      override def toString = s"LazyRef($ref)"
      override def equals(other: Any) = other match {
        case other: LazyRef => this.ref.equals(other.ref)
        case _              => false
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

    class CachedRefinedType(parent: Type, refinedName: Name, infoFn: RefinedType => Type) extends RefinedType(parent, refinedName) {
      val refinedInfo = infoFn(this)
    }

    class PreHashedRefinedType(parent: Type, refinedName: Name, override val refinedInfo: Type)
      extends RefinedType(parent, refinedName)

    object RefinedType {
      def make(parent: Type, names: List[Name], infoFns: List[RefinedType => Type]): Type =
        if (names.isEmpty) parent
        else make(RefinedType(parent, names.head, infoFns.head), names.tail, infoFns.tail)

      def apply(parent: Type, name: Name, infoFn: RefinedType => Type): RefinedType = {
        new CachedRefinedType(parent, name, infoFn)
      }

      def apply(parent: Type, name: Name, info: Type): RefinedType = {
        new PreHashedRefinedType(parent, name, info)
      }
    }

    trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
      def tp1: Type
      def tp2: Type
      def isAnd: Boolean
    }

    trait MethodicType extends Type {

      protected[TTypes] var mySignature: Signature = _
      //TODO add override methods to subtypes
      protected def computeSignature: Signature
      final override def signature: Signature =
        if (mySignature ne null) {
          mySignature
        } else computeSignature

      protected def resultSignature = try resultType match {
        case rtp: MethodicType => rtp.signature
        case tp => Signature(initGType)
      }
      catch {
        case ex: AssertionError =>
          println(i"failure while taking result signture of $this: $resultType")
          throw ex
      }
    }

    trait MethodOrPoly extends MethodicType

    abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
      extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>
      import MethodType._

      def isJava = false
      def isImplicit = false

      private[core] val resType = resultTypeExp(this)
      assert(resType.exists)

      override def resultType: Type = {      
        //TODO - in dotty implementation there is a spicial processing for false dependencies
        resType
      }

      //TODO - check correctness
      protected def computeSignature: Signature = {
        if (mySignature ne null) mySignature 
        else {
          mySignature = Signature(this.initGType)
          mySignature
        }
      }
        //resultSignature.prepend(paramTypes, isJava)

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
      def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type): MethodType
      def apply(paramNames: List[TermName], paramTypes: List[Type], resultType: Type): MethodType =
        apply(paramNames, paramTypes)(_ => resultType)
    }

    object MethodType extends MethodTypeCompanion {
      def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) =
        new CachedMethodType(paramNames, paramTypes)(resultTypeExp)
    }

    object JavaMethodType extends MethodTypeCompanion {
      def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) =
        new JavaMethodType(paramNames, paramTypes)(resultTypeExp)
    }

    object ImplicitMethodType extends MethodTypeCompanion {
      def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) =
        new ImplicitMethodType(paramNames, paramTypes)(resultTypeExp)
    }

    abstract case class ExprType(resType: Type)
      extends CachedProxyType with TermType with MethodicType {
      protected def computeSignature: Signature = {
        if (mySignature ne null) mySignature
        else {
          mySignature = resultSignature
          mySignature
        }
      }
      override def resultType: Type = resType
      override def underlying: Type = resType
    }

    final class CachedExprType(resultType: Type) extends ExprType(resultType)

    object ExprType {
      def apply(resultType: Type) = {
        //assertUnerased()
        new CachedExprType(resultType)
      }
    }

    case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
      extends CachedGroundType with BindingType with TermType with MethodOrPoly {

      val paramBounds = paramBoundsExp(this)
      val resType = resultTypeExp(this)

      override def resultType = resType

      protected def computeSignature = {
        if (mySignature ne null) mySignature
        else {
          mySignature = resultSignature
          mySignature
        }
      }

      // need to override hashCode and equals to be object identity
      // because paramNames by itself is not discriminatory enough
      override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]

      override def toString = s"PolyType($paramNames, $paramBounds, $resType)"
    }

    object PolyType {
      def fromSymbols(tparams: List[(Symbol, TypeBounds)], resultType: Type) =
        if (tparams.isEmpty) resultType
        else {
          //TODO PolyParam (how to process subst)
          //def transform(pt: PolyType, tp: Type) =
          //  tp.subst(tparams, (0 until tparams.length).toList map (PolyParam(pt, _)))
          apply(tparams map (_._1.name.asTypeName))( //names
            pt => tparams map (_._2), //type bounds
            pt => resultType)
        }
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

    class MethodParamImpl(binder: MethodType, paramNum: Int) extends MethodParam(binder, paramNum)

    object MethodParam {
      def apply(binder: MethodType, paramNum: Int): MethodParam = {
        //assertUnerased()
        new MethodParamImpl(binder, paramNum)
      }
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
        case _                 => false
      }
      override def toString = s"RefinedThis(${binder.hashCode})"
    }

    abstract case class SkolemType(info: Type) extends CachedProxyType with ValueType with SingletonType {
      override def underlying = info
      override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
      override def toString = s"Skolem($info)"
    }

    final class CachedSkolemType(info: Type) extends SkolemType(info)

    object SkolemType {
      def apply(info: Type) =
        new CachedSkolemType(info)
    }

    final class TypeVar(val origin: PolyParam /*, creatorState: TyperState, val owningTree: untpd.Tree*/ , val owner: Symbol) extends CachedProxyType with ValueType {
      override def stripTypeVar: Type = ???

      override def underlying: Type = ???

      override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

      override def toString = {
        s"TypeVar($origin)"
      }
    }

    abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      classParents: List[TypeRef],
      decls: Iterable[Symbol] /*Scope*/ ,
      selfInfo: DotClass /* should be: Type | Symbol */ ) extends CachedGroundType with TypeType {

      def typeRef: Type = ???

      override def toString = s"ClassInfo($prefix, $cls)"
    }

    final class CachedClassInfo(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Iterable[Symbol], selfInfo: DotClass)
      extends ClassInfo(prefix, cls, classParents, decls, selfInfo)

    object ClassInfo {
      def apply(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Iterable[Symbol], selfInfo: DotClass = NoType) =
        new CachedClassInfo(prefix, cls, classParents, decls, selfInfo)
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

    class RealTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

    abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias)

    class CachedTypeAlias(alias: Type, variance: Int) extends TypeAlias(alias, variance)

    object TypeBounds {
      def NothTpe = convertType(self.global.definitions.NothingTpe)
      def AnyTpe = convertType(self.global.definitions.AnyTpe)

      def apply(lo: Type, hi: Type): TypeBounds =
        new RealTypeBounds(lo, hi)
      def empty = apply(NothTpe, AnyTpe)
      def upper(hi: Type) = {
        apply(NothTpe, hi)
      }
      def lower(lo: Type) = {
        apply(lo, AnyTpe)
      }
    }

    object TypeAlias {
      def apply(alias: Type, variance: Int = 0) = new CachedTypeAlias(alias, variance)
      def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
    }

    case class AnnotatedType(annot: Annotation, tpe: Type)
      extends UncachedProxyType with ValueType {
      override def underlying: Type = tpe

      override def stripAnnots: Type = tpe.stripAnnots

      override def stripTypeVar: Type = ???
    }

    object AnnotatedType {
      def make(annots: List[Annotation], underlying: Type) =
        if (annots.isEmpty) underlying
        else (underlying /: annots)((tp, ann) => AnnotatedType(ann, tp))
    }

    abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType
    final class CachedJavaArrayType(elemType: Type) extends JavaArrayType(elemType)
    object JavaArrayType {
      def apply(elemType: Type) = new CachedJavaArrayType(elemType)
    }

    case class ImportType(expr: Tree) extends UncachedGroundType

    case object NoType extends CachedGroundType {
      override def exists = false
    }

    //for usage during type conversion, can't be presented during tree pickling
    case object IncompleteType extends CachedGroundType {
      override def exists = false
    }

    case object NoPrefix extends CachedGroundType

    abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType

    final class CachedWildcardType(optBounds: Type) extends WildcardType(optBounds)

    object WildcardType extends WildcardType(NoType) {
      def apply(bounds: TypeBounds) = new CachedWildcardType(bounds)
    }

    implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
  }
}
