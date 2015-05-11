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

class Plugin(val global: Global) extends NscPlugin { //with TastyPhase{
  val name = "tasty"
  val description = """Pickles Scala trees (tasty format).
  For more information visit https://github.com/VladimirNik/tasty"""

  object tastyGenBCode extends {
    override val global: Plugin.this.global.type = Plugin.this.global
  } with scala.tasty.internal.scalac.gencode.GenBCode2(global) //with scala.tools.nsc.Global$genBCode$(global) with scala.tasty.internal.scalac.gencode.GenBCode2

  val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
  genBCodeField.setAccessible(true)
  genBCodeField.set(global, tastyGenBCode)

  // update genBCode (jvm) in phasesSet
  val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
  val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
  val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
  phasesDescMap(PluginComponent) = "let our powers combine"
  val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
  if (phasesSet.exists(_.phaseName.contains("jvm"))) { // `scalac -help` doesn't instantiate standard phases
    def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
    val oldScs @ List(oldJVM) = List(subcomponentNamed("jvm"))
    val newScs = List(tastyGenBCode)
    def hijackDescription(pt: SubComponent, sc: SubComponent) = phasesDescMap(sc) = phasesDescMap(pt) + " with tasty"
    oldScs zip newScs foreach { case (pt, sc) => hijackDescription(pt, sc) }
    phasesSet --= oldScs
    phasesSet ++= newScs
  }
  ()

  val components = List[NscPluginComponent](/*TastyComponent, */ PluginComponent)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsBefore = List("jvm")
    override val runsAfter = List("dce")
    val phaseName = "tastygeneration"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("!!! Phase is running !!!")
      }
    }
  }
}