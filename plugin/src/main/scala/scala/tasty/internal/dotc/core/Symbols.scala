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

    private[this] var myDenot: SymDenotation = _
    private[core] def denot_=(d: SymDenotation) = myDenot = d
    final def denot: SymDenotation = myDenot
    final def asType: TypeSymbol = { assert(denot.isType, s"isType called on not-a-Type $this"); asInstanceOf[TypeSymbol] }
    final def name: ThisName = denot.name.asInstanceOf[ThisName]
    def pos: Position = if (coord.isPosition) coord.toPosition else NoPosition

    protected def prefixString = "Symbol"
    override def toString: String = myDenot.toString()
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
