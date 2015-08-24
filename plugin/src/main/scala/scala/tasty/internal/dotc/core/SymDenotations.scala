package scala.tasty.internal
package dotc
package core

trait TSymDenotations {
  self: API =>

  import Contexts._, Symbols._, Denotations._, Names._, NameOps._, Annotations._
  import Types._, Flags._, Decorators._, StdNames._
  import NameOps._
  import collection.mutable
  import collection.immutable.BitSet
  import scala.reflect.io.AbstractFile
  import ast._
  import annotation.tailrec
  import config.Printers._

  import SymDenotations._
  import self.global.{ Symbol => GSymbol }

  def SymDenotation(
    symbol: Symbol,
    owner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initGSymbol: GSymbol,
    initPrivateWithin: Symbol = NoSymbol): SymDenotation = {
    val result =
      if (symbol.isClass)
        if (initFlags is Package) new PackageClassDenotation(symbol, owner, name, initFlags, initGSymbol, initPrivateWithin)
        else new ClassDenotation(symbol, owner, name, initFlags, initGSymbol, initPrivateWithin)
      else new SymDenotation(symbol, owner, name, initFlags, initGSymbol, initPrivateWithin)
    result
  }

  object SymDenotations {
    class SymDenotation private[TSymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      final val name: Name,
      initFlags: FlagSet,
      val initGSymbol: GSymbol,
      initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol) {

      private[SymDenotations] val myFlags: FlagSet = adaptFlags(initFlags)

      def owner: Symbol = ownerIfExists

      final def flags: FlagSet = myFlags

      final def is(fs: FlagSet) = {
        (if (fs <= FromStartFlags) myFlags else flags) is fs
      }

      final def is(fs: FlagSet, butNot: FlagSet) =
        (if (fs <= FromStartFlags && butNot <= FromStartFlags) myFlags else flags) is (fs, butNot)

      private def adaptFlags(flags: FlagSet) = if (isType) flags.toTypeFlags else flags.toTermFlags

      //private[this] var myInfo: Type = ???

      private[this] var myInfo: Type = _
      private[this] var myAnnotations: List[Annotation] = Nil

      final def info: Type = {
        initGSymbol match {
          //if initGSymbol == g.NoSymbol set to NoType
          case self.global.NoSymbol => NoType
          case _ =>
            // should be computed based on symbol's initGSymbol and set in myInfo (should be lazy)
            myInfo = convertType(initGSymbol.info)
            myInfo
        }
      }

      final def privateWithin: Symbol = initPrivateWithin

      final def annotations: List[Annotation] = myAnnotations

      private[core] final def annotations_=(annots: List[Annotation]): Unit =
        myAnnotations = annots

      final def originalName =
        if (hasExpandedName) name.unexpandedName else name // !!!DEBUG, was: effectiveName

      def fullNameSeparated(separator: String): Name = {
        var sep = separator
        var stopAtPackage = false
        if (sep.isEmpty) {
          sep = "$"
          stopAtPackage = true
        }
        if (symbol == NoSymbol ||
          owner == NoSymbol ||
          owner.isEffectiveRoot ||
          stopAtPackage && owner.is(PackageClass)) name
        else {
          var encl = owner
          while (!encl.isClass && !encl.isPackageObject) {
            encl = encl.owner
            sep += "~"
          }
          if (owner.is(ModuleClass, butNot = Package) && sep == "$") sep = "" // duplicate scalac's behavior: don't write a double '$$' for module class members.
          val fn = encl.fullNameSeparated(separator) ++ sep ++ name
          if (isType) fn.toTypeName else fn.toTermName
        }
      }

      //TODO check/improve this implementation
      final def hasExpandedName: Boolean = isExpandedName(name)

      override def isType: Boolean = name.isTypeName

      final def isClass: Boolean = isInstanceOf[ClassDenotation]

      final def isRoot: Boolean =
        (name.toTermName == nme.ROOT || name == nme.ROOTPKG) && (owner eq NoSymbol)

      final def isEmptyPackage: Boolean =
        name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

      final def isEffectiveRoot = isRoot || isEmptyPackage

      final def isSetter: Boolean =
        (this is Accessor) &&
          //TODO - do we need original name here?
          //if (initial is ExpandedName) initial.name.unexpandedName else initial.name
          /*originalName*/ name.isSetterName
          //(!isCompleted || info.firstParamTypes.nonEmpty)

      final def isConstructor = name.isConstructorName

      def isPackageObject: Boolean = {
        val poName = if (isType) nme.PACKAGE_CLS else nme.PACKAGE
        (name.toTermName == poName) && (owner is Package) && (this is Module)
      }

      def thisType: Type = NoPrefix

      override def typeRef: TypeRef =
        TypeRef(owner.thisType, name.asTypeName, this)

      override def termRef: TermRef =
        TermRef(owner.thisType, name.asTermName, this)

      override def valRef: TermRef =
        TermRef.withSigAndDenot(owner.thisType, name.asTermName, Signature.NotAMethod, this)

      override def termRefWithSig: TermRef =
        TermRef.withSigAndDenot(owner.thisType, name.asTermName, signature, this)

      def nonMemberTermRef: TermRef =
        TermRef.withFixedSym(owner.thisType, name.asTermName, symbol.asTerm)

      final def moduleClass: Symbol = {
        def notFound = { println(s"missing module class for $name: $info"); NoSymbol }
        if (this is ModuleVal)
          info /*myInfo*/ match {
            case info: TypeRef => info.symbol
            case ExprType(info: TypeRef) => info.symbol // needed after uncurry, when module terms might be accessor defs
            //TODO fix if required
            //case info: LazyType => info.moduleClass
            case t: MethodType =>
              t.resultType match {
                case info: TypeRef => info.symbol
                case _             => notFound
              }
            case _ => notFound
          }
        else NoSymbol
      }

      def typeParamCreationFlags: FlagSet = TypeParam

      override def toString = {
        val kindString =
          if (myFlags is ModuleClass) "module class"
          else if (isClass) "class"
          else if (isType) "type"
          else if (myFlags is Module) "module"
          else if (myFlags is Method) "method"
          else "val"
        s"$kindString $name"
      }
    }

    class ClassDenotation private[TSymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      name: Name,
      initFlags: FlagSet,
      initGSymbol: GSymbol,
      initPrivateWithin: Symbol /*,
      initRunId: RunId*/ )
      extends SymDenotation(symbol, ownerIfExists, name, initFlags, initGSymbol, initPrivateWithin) {
      private[this] var myThisType: Type = null

      override def thisType: Type = {
        if (myThisType == null) myThisType = computeThisType
        myThisType
      }

      private def computeThisType: Type =
        ThisType.raw(
          TypeRef(if (this is Package) NoPrefix else owner.thisType, symbol.asType))

      private[this] var myTypeRef: TypeRef = null

      override def typeRef: TypeRef = {
        if (myTypeRef == null) myTypeRef = super.typeRef
        myTypeRef
      }

      final override def typeParamCreationFlags = ClassTypeParamCreationFlags
    }

    class PackageClassDenotation private[TSymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      name: Name,
      initFlags: FlagSet,
      initGSymbol: GSymbol,
      initPrivateWithin: Symbol /*,
    initRunId: RunId*/ )
      extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initGSymbol, initPrivateWithin /*, initRunId*/ ) {
    }

    class NoDenotation extends SymDenotation(
      NoSymbol, NoSymbol, "<none>".toTermName, Permanent, self.global.NoSymbol) {
      override def exists = false
      override def isTerm = false
      override def isType = false
      override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
      override def computeAsSeenFrom(pre: Type): SingleDenotation = this
    }

    val NoDenotation = new NoDenotation
  }
}