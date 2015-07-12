package scala.tasty.internal
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
  class Symbol private[Symbols] (val coord: Coord) extends DotClass {
    type ThisName <: Name

    final def denot: SymDenotation = ???
    final def asType: TypeSymbol = ??? //{ assert(isType, s"isType called on not-a-Type $this"); asInstanceOf[TypeSymbol] }
    final def name: ThisName = ???
    def pos: Position = if (coord.isPosition) coord.toPosition else NoPosition

    protected def prefixString = "Symbol"
    override def toString: String = ???
  }

  type TermSymbol = Symbol { type ThisName = TermName }
  type TypeSymbol = Symbol { type ThisName = TypeName }

  class ClassSymbol private[Symbols] (coord: Coord, val assocFile: AbstractFile)
    extends Symbol(coord) {

    type ThisName = TypeName

    final def classDenot: ClassDenotation =
      denot.asInstanceOf[ClassDenotation]

    override protected def prefixString = "ClassSymbol"
  }

  object NoSymbol extends Symbol(NoCoord)
  
  implicit def toDenot(sym: Symbol): SymDenotation = sym.denot
  implicit def toClassDenot(cls: ClassSymbol): ClassDenotation = cls.classDenot
}
