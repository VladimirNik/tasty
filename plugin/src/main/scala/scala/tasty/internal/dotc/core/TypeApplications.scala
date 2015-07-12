package scala.tasty.internal.dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import Names._
import NameOps._
import Flags._
import StdNames._
import util.Positions.Position
import config.Printers._
import collection.mutable

/** A decorator that provides methods for modeling type application */
class TypeApplications(val self: Type) extends AnyVal {

  /** Is type `tp` a Lambda with all Arg$ fields fully instantiated? */
  def isInstantiatedLambda: Boolean = ???

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   *  @param interpolate   See argInfo
   */
  final def argInfos(interpolate: Boolean): List[Type] = ???

  final def argInfos: List[Type] = argInfos(interpolate = true)

  /** The core type without any type arguments.
   *  @param `typeArgs` must be the type arguments of this type.
   */
  final def withoutArgs(typeArgs: List[Type]): Type = typeArgs match {
    case _ :: typeArgs1 =>
      val RefinedType(tycon, _) = self
      tycon.withoutArgs(typeArgs1)
    case nil =>
      self
  }
}
