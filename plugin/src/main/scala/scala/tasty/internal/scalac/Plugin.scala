package scala.tasty
package internal.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import phase.TastyPhase
import scala.collection.mutable
import scala.tools.nsc.SubComponent
import scala.tools.nsc.Phase
import scala.tools.nsc.backend.jvm.GenBCode
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.transform.Erasure

class Plugin(val global: Global) extends NscPlugin with TastyPhase {
  val name = "tasty"
  val description = """Pickles Scala trees (tasty format).
  For more information visit https://github.com/VladimirNik/tasty"""

  def changePhasesOrder(runsAfterPhase: String, phaseClass: Class[_], fieldToModify: Object) = {
    val newRunsAfter = List(runsAfterPhase)
    val runsAfterField = phaseClass.getDeclaredField("runsAfter")
    runsAfterField.setAccessible(true)
    runsAfterField.set(fieldToModify, newRunsAfter)
  }

  //update the order of phases
  //typer, superaccessors
  changePhasesOrder("typer", classOf[scala.tools.nsc.Global$superAccessors$], global.superAccessors)
  
  //superaccessors, patmat
  changePhasesOrder("superaccessors", classOf[scala.tools.nsc.Global$patmat$], global.patmat)
  
  //patmat, extensionMethods
  changePhasesOrder("patmat", classOf[scala.tools.nsc.Global$extensionMethods$], global.extensionMethods)

  val components = List[NscPluginComponent](TastyComponent)
}