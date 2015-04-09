package scala.tasty.internal.scalac
package util

import scala.tools.nsc.Global
import scala.language.implicitConversions

trait TastyUtils {
  val global: Global
  import global.{newTermName, Position, NoPosition, Name}

  implicit def toTermName(str: String) = newTermName(str)
  
  def termName(bs: Array[Byte], offset: Int, len: Int) = newTermName(bs, offset, len)

  private[this] val SHADOWED: Name = "(shadowed)"
  def isShadowedName(name: Name) = name.length > 0 && name.startsWith('(') && name.startsWith(SHADOWED)

  def take(name: Name)(n: Int): Name = name.subName(0, n)

  def drop(name: Name)(n: Int): Name = name.subName(n, name.length)

  def revertShadowed(name: Name): Name = (drop(name)(SHADOWED.length))

  import global.{TypeName, MethodSymbol, Type, PolyType, MethodType, tpnme}

  case class Signature(paramsSig: List[TypeName], resSig: TypeName) {
    def notAMethod = paramsSig.isEmpty && resSig.isEmpty
  }

  object Signature {
    def apply(tpe: Type): Signature = {
      tpe match {
        case _: MethodType | _: PolyType =>
          //TODO - add erasure
          val paramsSig = tpe.paramss.flatten map {_.tpe.typeSymbol.fullNameAsName('.').toTypeName}
          val resSig = tpe.finalResultType.typeSymbol.fullNameAsName('.').toTypeName
          Signature(paramsSig, resSig)
        case _ => Signature(List(), tpnme.EMPTY.toTypeName)
      }
    }
  }
}