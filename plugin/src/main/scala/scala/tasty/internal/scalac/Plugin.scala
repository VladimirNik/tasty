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
import scala.tasty.internal.scalac.gencode.TastyGenPhase

class Plugin(val global: Global) extends NscPlugin with TastyPhase with TastyGenPhase {
  val name = "tasty"
  val description = """Pickles Scala trees (tasty format).
  For more information visit https://github.com/VladimirNik/tasty"""

  object tastyGenBCode extends {
    override val global: Plugin.this.global.type = Plugin.this.global
  } with TastyGenBCode(global) //with scala.tools.nsc.Global$genBCode$(global) with scala.tasty.internal.scalac.gencode.GenBCode2

  val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
  genBCodeField.setAccessible(true)
  genBCodeField.set(global, tastyGenBCode)

  // update genBCode (jvm) in phasesSet
  val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
  val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
  val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
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

  //update the order of phases
  //superaccessors
  //println(s"declared fields: ${classOf[Global].getDeclaredFields map {_.getName.trim } filter { _.contains("super") } toList }")
  //val superaccsField = classOf[Global].getDeclaredField("superAccessors$module")
  //superaccsField.setAccessible(true)
  //val superaccs = superaccsField.get(global)
  val newRunsAfterSup = List("typer")
  //println(s"declared fields: ${classOf[scala.tools.nsc.Global$superAccessors$].getDeclaredFields map {_.getName.trim } /*filter { _.contains("runsAfter") }*/ toList }")
  val runsAfterSupField = classOf[scala.tools.nsc.Global$superAccessors$].getDeclaredField("runsAfter")
  runsAfterSupField.setAccessible(true)
  runsAfterSupField.set(/*superaccs*/ global.superAccessors, newRunsAfterSup)
  
  //patmat
  val newRunsAfterPat = List("superaccessors")
  //println(s"declared fields: ${classOf[scala.tools.nsc.Global$superAccessors$].getDeclaredFields map {_.getName.trim } /*filter { _.contains("runsAfter") }*/ toList }")
  val runsAfterPatField = classOf[scala.tools.nsc.Global$patmat$].getDeclaredField("runsAfter")
  runsAfterPatField.setAccessible(true)
  runsAfterPatField.set(global.patmat, newRunsAfterPat)
  
  //extensionMethods
    val newRunsAfterEM = List("patmat")
  //println(s"declared fields: ${classOf[scala.tools.nsc.Global$superAccessors$].getDeclaredFields map {_.getName.trim } /*filter { _.contains("runsAfter") }*/ toList }")
  val runsAfterEMField = classOf[scala.tools.nsc.Global$extensionMethods$].getDeclaredField("runsAfter")
  runsAfterEMField.setAccessible(true)
  runsAfterEMField.set(global.extensionMethods, newRunsAfterEM)

  val components = List[NscPluginComponent](TastyComponent, AfterPatmatComponent, AfterSAComponent)

  object AfterSAComponent extends {
    val runsAfter = List("superaccessors")
    val global: this.global.type = this.global
  } with NscPluginComponent {
    override val runsRightAfter = Some("superaccessors")
    val phaseName = "after-superaccessors"
    override def description = "pickle tasty trees"

    import global._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("Run after superaccessors!")
      }
    }
  }

  object AfterPatmatComponent extends {
    val runsAfter = List("patmat")
    val global: this.global.type = this.global
  } with NscPluginComponent {
    override val runsRightAfter = Some("patmat")
    val phaseName = "after-patmat"
    override def description = "pickle tasty trees"

    import global._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("Run after patmat!")
      }
    }
  }

  object AfterTyperComponent extends {
    val runsAfter = List("typer")
    val global: this.global.type = this.global
  } with NscPluginComponent {
    override val runsRightAfter = Some("typer")
    val phaseName = "after-typer"
    override def description = "pickle tasty trees"

    import global._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        println("Run after typer!")
      }
    }
  }
}