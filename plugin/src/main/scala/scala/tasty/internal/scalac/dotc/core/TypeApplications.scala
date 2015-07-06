package scala.tasty.internal.scalac.dotc
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

object TypeApplications {

  /** Assert type is not a TypeBounds instance and return it unchanged */
  val noBounds = (tp: Type) => tp match {
    case tp: TypeBounds => throw new AssertionError("no TypeBounds allowed")
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its lower bound else return `tp` */
  val boundsToLo = (tp: Type) => tp match {
    case tp: TypeBounds => tp.lo
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its upper bound else return `tp` */
  val boundsToHi = (tp: Type) => tp match {
    case tp: TypeBounds => tp.hi
    case _ => tp
  }
}

import TypeApplications._

/** A decorator that provides methods for modeling type application */
class TypeApplications(val self: Type) extends AnyVal {

  /** The type parameters of this type are:
   *  For a ClassInfo type, the type parameters of its class.
   *  For a typeref referring to a class, the type parameters of the class.
   *  For a typeref referring to an alias or abstract type, the type parameters of
   *    its right hand side or upper bound.
   *  For a refinement type, the type parameters of its parent, unless the refinement
   *  re-binds the type parameter with a type-alias.
   *  For any other non-singleton type proxy, the type parameters of its underlying type.
   *  For any other type, the empty list.
   */
  final def typeParams(implicit ctx: Context): List[TypeSymbol] = /*>|>*/ /*track("typeParams")*/ /*<|<*/ {
    self match {
      case tp: ClassInfo =>
        tp.cls.typeParams
      case tp: TypeRef =>
        val tsym = tp.typeSymbol
        if (tsym.isClass) tsym.typeParams
        else tp.underlying.typeParams
      case tp: RefinedType =>
        val tparams = tp.parent.typeParams
        tp.refinedInfo match {
          case rinfo: TypeAlias => tparams.filterNot(_.name == tp.refinedName)
          case _ => tparams
        }
      case tp: SingletonType =>
        Nil
      case tp: TypeProxy =>
        tp.underlying.typeParams
      case _ =>
        Nil
    }
  }

  /** Is type `tp` a Lambda with all Arg$ fields fully instantiated? */
  def isInstantiatedLambda(implicit ctx: Context): Boolean = ???

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   *  @param interpolate   See argInfo
   */
  final def argInfos(interpolate: Boolean)(implicit ctx: Context): List[Type] = ???

  final def argInfos(implicit ctx: Context): List[Type] = argInfos(interpolate = true)

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
