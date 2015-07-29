package scala.tasty.internal.dotc
package ast

import util.Positions._

abstract class Positioned extends util.DotClass with Product {
  private[this] var curPos: Position = _

  def pos: Position = curPos

  def withPos(pos: Position): Unit = {
    curPos = pos
  }
}
