package scala.tasty.internal.scalac
package util

import scala.tools.nsc.Global
import scala.language.implicitConversions

trait TastyUtils {
  val global: Global
  import global.{newTermName, Position, NoPosition, Name}

  //TODO - organize NameOps
  implicit def toTermName(str: String) = newTermName(str)
  
  def termName(bs: Array[Byte], offset: Int, len: Int) = newTermName(bs, offset, len)

  private[this] val SHADOWED: Name = "(shadowed)"
  def isShadowedName(name: Name) = name.length > 0 && name.startsWith('(') && name.startsWith(SHADOWED)

  def take(name: Name)(n: Int): Name = name.subName(0, n)

  def drop(name: Name)(n: Int): Name = name.subName(n, name.length)

  //TODO - likeTyped transformation removed from revertShadowed
  def revertShadowed(name: Name): Name = (drop(name)(SHADOWED.length))
}