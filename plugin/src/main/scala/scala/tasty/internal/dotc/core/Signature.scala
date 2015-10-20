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
        case _: self.g.MethodType | _: self.g.PolyType | _: self.g.NullaryMethodType =>
          //TODO - add erasure
          import self.GlobalToTName._
          val paramsSig = tpe.paramss.flatten map { param => convertTypeName(genTypeName(param.tpe.erasure.typeSymbol)) }
          val frts = tpe.finalResultType.erasure.typeSymbol
          val resSig = genTypeName(frts)
          Signature(paramsSig, resSig)
        case _ => NotAMethod
      }
    }

    def genTypeName(frts: self.g.Symbol): self.g.TypeName = {
      // in dotty objects are represented as TypeDef with ValDef
      // this TypeDef should be emulated during the transformation
      // if there is a usage of such TypeDef in signature
      // its TypeName also should be emulated
      def genNameThatContainsModule(mts: self.g.Symbol): self.g.TypeName = {
        val synthTpName = if (mts.isModuleClass) syntheticName(mts.name) else mts.name
        (mts.owner match {
          case fo if fo.isEmptyPackageClass =>
            self.g.tpnme.EMPTY
          case fo if fo.isPackageClass =>
            fo.fullNameAsName('.').toTypeName.append('.')
          case fo if fo.isModuleClass =>
            genNameThatContainsModule(fo).append('.')
            //TODO - check this use case for empty package, root package
            //this branch is for cases similar to object t { class t1 { object t2 { class t3 } } }
          case fo if fo.ownerChain.exists(sym => sym.isModuleClass && !sym.isPackageClass) =>
            genNameThatContainsModule(fo.owner).append('.').append(fo.name).append('.')
          case fo =>
            fo.fullNameAsName('.').toTypeName.append('.') // change here just to name
        }).append(synthTpName)
      }
      val resSig = if (frts.isModuleClass || frts.ownerChain.exists(_.isModuleClass)) {
        genNameThatContainsModule(frts)
      } else frts.fullNameAsName('.').toTypeName
      resSig
    }
  }
}