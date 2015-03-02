package scala.tasty.internal.scalac
package util

import scala.tools.nsc.Global
import scala.language.implicitConversions

trait TastyUtils {
  val global: Global
  import global.{newTermName, Position, NoPosition}
  
  implicit def toTermName(str: String) = newTermName(str)
  
  def termName(bs: Array[Byte], offset: Int, len: Int) = newTermName(bs, offset, len)
}