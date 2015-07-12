package scala.tasty.internal.dotc
package ast

import util.Positions._

abstract class Positioned extends util.DotClass with Product {
  def pos: Position = ???
}
