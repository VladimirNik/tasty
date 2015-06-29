package scala.tasty.internal.scalac.pickler.core

import scala.tools.nsc.Global

trait TastyTypes {
  val global: Global
  import global.{ Type, Symbol, Name, TypeRef }
  
  case class TermRef(termSym: Symbol) extends Type
  case class ModuleType(pre: Type, sym: Symbol, modName: Name) extends Type
}