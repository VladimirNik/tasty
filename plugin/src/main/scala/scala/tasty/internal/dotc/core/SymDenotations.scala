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

  trait L { this: Context =>
    import SymDenotations._

    def SymDenotation(
      symbol: Symbol,
      owner: Symbol,
      name: Name,
      initFlags: FlagSet,
      initInfo: Type,
      initPrivateWithin: Symbol = NoSymbol): SymDenotation = ???
  }

  object SymDenotations {
    class SymDenotation private[SymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      final val name: Name,
      initFlags: FlagSet,
      initInfo: Type,
      initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol) {

      private[this] var myFlags: FlagSet = ??? //adaptFlags(initFlags)

      def owner: Symbol = ??? //ownerIfExists

      final def flags: FlagSet = ???

      final def is(fs: FlagSet) = {
        (if (fs <= FromStartFlags) myFlags else flags) is fs
      }

      final def info: Type = ???

      final def privateWithin: Symbol = ???

      final def annotations: List[Annotation] = ???

      override def isType: Boolean = name.isTypeName

      final def isClass: Boolean = isInstanceOf[ClassDenotation]

      final def isRoot: Boolean =
        (name.toTermName == nme.ROOT || name == nme.ROOTPKG) && (owner eq NoSymbol)

      final def isEmptyPackage: Boolean =
        name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

      final def isEffectiveRoot = isRoot || isEmptyPackage

      final def isSetter: Boolean = ???

      final def isConstructor = name.isConstructorName

      override def typeRef: TypeRef = ???

      override def termRef: TermRef = ???

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

    class ClassDenotation private[SymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      name: Name,
      initFlags: FlagSet,
      initInfo: Type,
      initPrivateWithin: Symbol /*,
    initRunId: RunId*/ )
      extends SymDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin)

    class PackageClassDenotation private[SymDenotations] (
      symbol: Symbol,
      ownerIfExists: Symbol,
      name: Name,
      initFlags: FlagSet,
      initInfo: Type,
      initPrivateWithin: Symbol /*,
    initRunId: RunId*/ )
      extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin /*, initRunId*/ ) {
    }

    class NoDenotation extends SymDenotation(
      NoSymbol, NoSymbol, "<none>".toTermName, Permanent, NoType) {
      override def exists = false
      override def isTerm = false
      override def isType = false
      override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    }

    val NoDenotation = new NoDenotation
  }
}