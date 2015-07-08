package scala.tasty.internal.scalac.dotc
package ast

import util.Positions._

/** A base class for things that have positions (currently: modifiers and trees)
 */
abstract class Positioned extends util.DotClass with Product {
//  private[this] var curPos: Position = _

  /** The item's position.
   */
  def pos: Position = ??? //curPos
}
