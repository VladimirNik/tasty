package scala.tasty.internal
package dotc
package core

trait TSymbols {
  self: API =>

  import Names._
  import Flags._
  import java.lang.AssertionError
  import Decorators._
  import Symbols._
  import Contexts._
  import printing.Texts._
  import Types._
  import Annotations._
  import util.Positions._
  import StdNames._
  import NameOps._
  import ast.tpd.Tree
  import collection.mutable
  import language.implicitConversions
  import scala.reflect.io.AbstractFile
  import SymDenotations._
  import util.DotClass

  //object core is created because we don't want to change code inside the pickling
  object core {
    object Symbols {
      type Symbol = self.Symbols.Symbol
    }
  }
  
  def newNakedSymbol[N <: Name](coord: Coord = NoCoord): Symbol { type ThisName = N } =
    new Symbol(coord).asInstanceOf[Symbol { type ThisName = N }]

  def newNakedClassSymbol(coord: Coord = NoCoord, assocFile: AbstractFile = null) =
    new ClassSymbol(coord, assocFile)

  import self.global.{ Symbol => GSymbol }
  
  def newSymbol[N <: Name](
    owner: Symbol,
    name: N,
    flags: FlagSet,
    initGSymbol: GSymbol,
    privateWithin: Symbol = NoSymbol,
    coord: Coord = NoCoord): Symbol { type ThisName = N } = {
    val sym = newNakedSymbol[N](coord)
    val denot = SymDenotation(sym, owner, name, flags, initGSymbol, privateWithin)
    sym.denot = denot
    sym
  }

  def newClassSymbol(
    owner: Symbol,
    name: TypeName,
    flags: FlagSet,
    initGSymbol: GSymbol,
    privateWithin: Symbol = NoSymbol,
    coord: Coord = NoCoord,
    assocFile: AbstractFile = null): ClassSymbol = {
    val cls = newNakedClassSymbol(coord, assocFile)
    val denot = SymDenotation(cls, owner, name, flags, initGSymbol, privateWithin)
    cls.denot = denot
    cls
  }

  //TODO should be implemented for Modules (see in Dotty implementation)
  //def newModuleSymbol

  //TODO see for package objects original dotty implementation
  def newPackageSymbol(
    owner: Symbol,
    name: TermName,
    flags: FlagSet = EmptyFlags,
    initGSymbol: GSymbol): TermSymbol =
    newSymbol(owner, name, flags | PackageCreationFlags, initGSymbol)

  def newImportSymbol(owner: Symbol, initGSymbol: GSymbol, coord: Coord = NoCoord) =
    newSymbol(owner, nme.IMPORT, EmptyFlags, initGSymbol, coord = coord)

  def newConstructor(cls: Symbol, flags: FlagSet, initGSymbol: GSymbol, privateWithin: Symbol = NoSymbol, coord: Coord = NoCoord) =
    newSymbol(cls, nme.CONSTRUCTOR, flags | Method, initGSymbol, privateWithin, coord)

  def newDefaultConstructor(cls: ClassSymbol) =
    newConstructor(cls, EmptyFlags, self.global.NoSymbol)

  def newSelfSym(cls: ClassSymbol, name: TermName = nme.WILDCARD, initGSymbol: GSymbol): TermSymbol =
    newSymbol(cls, name, SelfSymFlags, initGSymbol, coord = cls.coord)

  object Symbols {
    class Symbol private[TSymbols] (val coord: Coord) extends DotClass {
      type ThisName <: Name

      private[this] var myDenot: SymDenotation = _
      private[core] def denot_=(d: SymDenotation) = myDenot = d
      //TODO - should be only final - see the problem with denot
      /*final*/ def denot: SymDenotation = myDenot

      final def isClass: Boolean = isInstanceOf[ClassSymbol]

      final def isTerm: Boolean =
        denot.isTerm

      final def isType: Boolean =
        denot.isType

      final def asTerm: TermSymbol = { assert(isTerm, s"asTerm called on not-a-Term $this"); asInstanceOf[TermSymbol] }
      final def asType: TypeSymbol = { assert(isType, s"isType called on not-a-Type $this"); asInstanceOf[TypeSymbol] }
      final def asClass: ClassSymbol = asInstanceOf[ClassSymbol]
      final def name: ThisName = denot.name.asInstanceOf[ThisName]
      def pos: Position = if (coord.isPosition) coord.toPosition else NoPosition

      protected def prefixString = "Symbol"
      override def toString: String = myDenot.toString()
    }

    type TermSymbol = Symbol { type ThisName = TermName }
    type TypeSymbol = Symbol { type ThisName = TypeName }

    class ClassSymbol private[TSymbols] (coord: Coord, val assocFile: AbstractFile)
      extends Symbol(coord) {

      type ThisName = TypeName

      final def classDenot: ClassDenotation =
        denot.asInstanceOf[ClassDenotation]

      override protected def prefixString = "ClassSymbol"
    }

    object NoSymbol extends Symbol(NoCoord) {
      override def denot = NoDenotation
      override def toString = "NoSymbol"
      //TODO problem with denot = ... (NoSymbol init)
      //denot = NoDenotation
    }

    object IncompleteSymbol extends Symbol(NoCoord) {
      override def denot = NoDenotation
      override def toString = "IncompleteSymbol"
    }
    
    implicit def toDenot(sym: Symbol): SymDenotation = sym.denot
    implicit def toClassDenot(cls: ClassSymbol): ClassDenotation = cls.classDenot

    //Moved from Decorators in order not to include it to cake
    /**
     * Implements a findSymbol method on iterators of Symbols that
     *  works like find but avoids Option, replacing None with NoSymbol.
     */
    implicit class SymbolIteratorDecorator(val it: Iterator[Symbol]) /* extends AnyVal */ {
      final def findSymbol(p: Symbol => Boolean): Symbol = {
        while (it.hasNext) {
          val sym = it.next
          if (p(sym)) return sym
        }
        NoSymbol
      }
    }
  }
}
