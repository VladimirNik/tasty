package scala.tasty.internal.scalac
package dotc
package core

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

object Symbols {

  /** A Symbol represents a Scala definition/declaration or a package.
   */
  class Symbol private[Symbols] (val coord: Coord) extends DotClass {

    type ThisName <: Name

    /** The current denotation of this symbol */
    final def denot(implicit ctx: Context): SymDenotation = ???

    final def asType(implicit ctx: Context): TypeSymbol = ??? //{ assert(isType, s"isType called on not-a-Type $this"); asInstanceOf[TypeSymbol] }

    /** The current name of this symbol */
    final def name(implicit ctx: Context): ThisName = ???

    /** The position of this symbol, or NoPosition is symbol was not loaded
     *  from source.
     */
    def pos: Position = if (coord.isPosition) coord.toPosition else NoPosition

// -------- Printing --------------------------------------------------------

    /** The prefix string to be used when displaying this symbol without denotation */
    protected def prefixString = "Symbol"

    override def toString: String = ???
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, val assocFile: AbstractFile)
    extends Symbol(coord) {

    type ThisName = TypeName

    final def classDenot(implicit ctx: Context): ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    override protected def prefixString = "ClassSymbol"
  }

  object NoSymbol extends Symbol(NoCoord)

  /** Makes all denotation operations available on symbols */
  implicit def toDenot(sym: Symbol)(implicit ctx: Context): SymDenotation = sym.denot

  /** Makes all class denotations available on class symbols */
  implicit def toClassDenot(cls: ClassSymbol)(implicit ctx: Context): ClassDenotation = cls.classDenot
}
