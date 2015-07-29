package scala.tasty.internal
package convert

import scala.tasty.internal.dotc.util.{ Positions => t }
import scala.language.implicitConversions

trait PositionConverter {
  self: API =>

  implicit def convertToTPosition(pos: g.Position): t.Position = {
    pos match {
      case g.NoPosition => t.NoPosition
      case _ if !pos.isDefined => t.NoPosition
      case _ => t.Position(pos.start, pos.end, pos.point)
    }
  }

  implicit def convertToTCoord(pos: g.Position): t.Coord = convertToTPosition(pos) 
}