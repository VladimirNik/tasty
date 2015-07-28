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
      initGSymbol: GSymbol,
      initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol) {

      private[SymDenotations] val myFlags: FlagSet = adaptFlags(initFlags)

      def owner: Symbol = ownerIfExists

      final def flags: FlagSet = myFlags

      final def is(fs: FlagSet) = {
        (if (fs <= FromStartFlags) myFlags else flags) is fs
      }

      private def adaptFlags(flags: FlagSet) = if (isType) flags.toTypeFlags else flags.toTermFlags

      //private[this] var myInfo: Type = ???

      //if initGSymbol == g.NoSymbol set to NoType
      final def info: Type = 
        if (initGSymbol == self.global.NoSymbol) NoType
        else ??? // should be computed based on symbol's initGSymbol and set in myInfo (should be lazy)
          
      final def privateWithin: Symbol = ??? //TODO fix PrivateWithin

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

      final def moduleClass: Symbol = {
        def notFound = { println(s"missing module class for $name: $info"); NoSymbol }
        if (this is ModuleVal)
          info /*myInfo*/ match {
            case info: TypeRef           => info.symbol
            case ExprType(info: TypeRef) => info.symbol // needed after uncurry, when module terms might be accessor defs
            //TODO fix if required
            //case info: LazyType          => info.moduleClass
            case t: MethodType =>
              t.resultType match {
                case info: TypeRef => info.symbol
                case _             => notFound
              }
            case _ => notFound
          }
        else NoSymbol
      }

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
      extends SymDenotation(symbol, ownerIfExists, name, initFlags, initGSymbol, initPrivateWithin)

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
    }

    
    val NoDenotation = new NoDenotation
  }
}