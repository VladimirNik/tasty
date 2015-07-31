package scala.tasty.internal
package dotc
package core

import Names._
import StdNames._

trait Signatures {
  self: API =>
  case class Signature(paramsSig: List[TypeName], resSig: TypeName)
  object Signature {
    val NotAMethod = Signature(List(), EmptyTypeName)
    def apply(tpe: self.g.Type): Signature = {
      tpe match {
        case _: self.g.MethodType | _: self.g.PolyType =>
          //TODO - add erasure
          import self.GlobalToTName._
          val paramsSig = tpe.paramss.flatten map { param => convertToTypeName(param.tpe.typeSymbol.fullNameAsName('.')) }
          val resSig = tpe.finalResultType.typeSymbol.fullNameAsName('.').toTypeName
          Signature(paramsSig, resSig)
        case _ => NotAMethod
      }
    }
  }
}