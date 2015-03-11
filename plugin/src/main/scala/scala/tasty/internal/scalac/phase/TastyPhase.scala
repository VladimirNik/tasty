package scala.tasty
package internal.scalac
package phase

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.reflect.io.AbstractFile
import scala.tasty.internal.scalac.pickler.core.TreePicklers

trait TastyPhase {
  self: Plugin =>

  object TastyComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._

    override val runsAfter = List("typer")
    override val runsRightAfter = None
    val phaseName = "tasty"
    override def description = "pickle tasty trees"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("<=== Tasty phase ===>")
        if (!unit.isJava) {
          val tree = unit.body
          val picklers = new TreePicklers {
            val global: self.global.type = self.global
          }
          val pickler = new picklers.TastyPickler            
          val treePkl = new picklers.TreePickler(pickler)
          treePkl.pickle(tree :: Nil)
        }
      }
    }
  }
}