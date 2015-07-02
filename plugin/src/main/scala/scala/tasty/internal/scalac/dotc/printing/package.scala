package scala.tasty.internal.scalac.dotc

import util.Chars._
import core.Names.Name
import core.StdNames.nme
import core.NameOps._

package object printing {

  type Precedence = Int

  def precedence(operator: Name): Int =
    if (operator eq nme.ERROR) -1
    else {
      val firstCh = operator(0)
      if (isScalaLetter(firstCh)) 1
      else if (operator.isOpAssignmentName) 0
      else firstCh match {
        case '|'             => 2
        case '^'             => 3
        case '&'             => 4
        case '=' | '!'       => 5
        case '<' | '>'       => 6
        case ':'             => 7
        case '+' | '-'       => 8
        case '*' | '/' | '%' => 9
        case _               => 10
      }
    }

  def minPrec = 0
  def minInfixPrec = 1
  def maxPrec = 11
  
  val DotPrec       = maxPrec
  val AndPrec       = precedence(nme.raw.AMP)
  val OrPrec        = precedence(nme.raw.BAR)
  val InfixPrec     = minInfixPrec
  val GlobalPrec    = minPrec
  val TopLevelPrec  = minPrec - 1

}
