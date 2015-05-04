package scala.tasty
package internal.scalac
package phase


import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.reflect.io.AbstractFile

trait GenCPhase {
  self: Plugin =>

  object GenCodeComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._

    override val runsAfter = List("icode")//List("jvm")
    override val runsRightAfter = Some("icode")
    override val runsBefore = List("jvm")
    val phaseName = "jvm2"
    override def description = "pickle tasty trees"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        System.out.println("jvm2: Invocation after jvm !!!")
      }
    }
  }
}