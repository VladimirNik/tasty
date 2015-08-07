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
    //TODO - try to create signature based on t.Type
    def apply(tpe: self.g.Type): Signature = {
      tpe match {
        case _: self.g.MethodType | _: self.g.PolyType =>
          //TODO - add erasure
          import self.GlobalToTName._
          val paramsSig = tpe.paramss.flatten map { param => convertToTypeName(param.tpe.typeSymbol.fullNameAsName('.')) }
          val frts = tpe.finalResultType.typeSymbol
          val resSig = if (frts.isModuleClass) {
            //result name of constructor type inside module class should be '$'resType
            val synthTpName = syntheticName(frts.name)
            frts.owner.fullNameAsName('.').toTypeName.append('.').append(synthTpName)
          } else frts.fullNameAsName('.').toTypeName
          Signature(paramsSig, resSig)
        case _ => NotAMethod
      }
    }
  }
}