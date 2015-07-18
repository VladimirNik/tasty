package scala.tasty.internal.dotc
package core

import Names._
import StdNames._

case class Signature(paramsSig: List[TypeName], resSig: TypeName)

object Signature {
  val NotAMethod = Signature(List(), EmptyTypeName)
}
