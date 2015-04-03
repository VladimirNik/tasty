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
        println(s"<=== Tasty phase: ${unit.toString()} ===>")
        val tree = unit.body
        
//        var debugCond = false
//        
//        import global.{showRaw, show, ValDef}
//
//        def setDebugCond(tr: Tree) = tr match {
//          case ValDef(_, name, _, _) if name.toString() == "testCodeInNextCases" => debugCond = true
//          case ValDef(_, name, _, _) if name.toString() =="body" => debugCond = false
//          case _ =>
//        }
//
//        def debug(str: String) = if (debugCond) println(str)
//        
//        val traverser = new global.Traverser {
//          override def traverse(tree: Tree) {
//            setDebugCond(tree)
//            debug("")
//            tree match {
//              case _ =>
//                debug(s"showCode(tree): ${showCode(tree)}")
//                debug(s"show(tree): ${show(tree)}")
//                debug(s"showRaw(tree): ${showRaw(tree, printTypes = true)}")
//                if (tree.symbol != null) {
//                  debug(s"showRaw(symbol): ${showRaw(tree.symbol, printTypes = true)}")
//                  debug(s"showRaw(symbol.info): ${showRaw(tree.symbol.info, printTypes = true)}")
//                  debug(s"showRaw(symbol.tpe): ${showRaw(tree.symbol.tpe, printTypes = true)}")
//                }
//                if (tree.tpe != null) {
//                  debug(s"showRaw(tree.tpe): ${showRaw(tree.tpe, printTypes = true)}")
//                }
//                super.traverse(tree)
//            }
//            debug("")
//          }
//        }
//        
//        traverser.traverse(tree)
        
        if (!unit.isJava) {
          val tree = unit.body
          val picklers = new {
            val global: self.global.type = self.global
          } with TreePicklers
          val pickler = new picklers.TastyPickler            
          val treePkl = new picklers.TreePickler(pickler)
          treePkl.pickle(tree :: Nil)
        }
      }
    }
  }
}