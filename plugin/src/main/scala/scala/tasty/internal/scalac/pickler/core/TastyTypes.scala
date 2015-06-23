package scala.tasty.internal.scalac.pickler.core

import scala.tools.nsc.Global

trait TastyTypes {
  val global: Global
  import global.{ Type, Symbol }
  
  case class TermRef(termSym: Symbol) extends Type
}