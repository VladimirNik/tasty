package scala.tasty
package internal.scalac

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import phase.TastyPhase
import scala.collection.mutable
import scala.tools.nsc.SubComponent
import scala.tools.nsc.Phase
import scala.tasty.internal.scalac.gencode.GenC
import scala.tools.nsc.backend.jvm.GenBCode

class Plugin(val global: Global) extends NscPlugin with TastyPhase with GenC{
  val name = "tasty"
  val description = """Pickles Scala trees (tasty format).
  For more information visit https://github.com/VladimirNik/tasty"""
  
  // install a pretty description for our plugin phase instead of empty string hardcoded for all plugins
  val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
  val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
  phasesDescMap(PluginComponent) = "let our powers combine"

  // replace Global.analyzer to customize namer and typer (step 1 of 3)
  // unfortunately compiler plugins are instantiated too late
  // therefore by now analyzer has already been used to instantiate the namer, packageobjects and typer subcomponents
  // these are not phases yet - they are just phase factories - so no disaster yet, but we have to be quick
  // this warrants the second step in this customization - rewiring phase factories
//  lazy val genBCode = new { 
//    val global: Plugin.this.global.type = Plugin.this.global
//    val runsAfter = List("dce")
//    val runsRightAfter = None  
//  } with GenBCode2
  object genBCode2 extends {
    val global: Plugin.this.global.type = Plugin.this.global
    val runsAfter = List("dce")
    val runsRightAfter = None  
  } with GenBCode
//  val analyzer = classOf[Global].getDeclaredField("analyzer")
//  println(classOf[Global].getDeclaredFields.toList filter (_.toString.contains("gen")))
  val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
  genBCodeField.setAccessible(true)
  genBCodeField.set(global, genBCode2)

  //  // replace Global.analyzer to customize namer and typer (step 2 of 3)
  //  // luckily for us compiler plugins are instantiated quite early
  //  // so by now internal phases have only left a trace in phasesSet and in phasesDescMap
  //  // also up until now noone has really used the standard analyzer, so we're almost all set
  //  // except for the standard `object typer extends analyzer.Typer(<some default context>)`
  //  // that is a member of Global and hence has been pre-initialized now
  //  // good news is that it's only used in later phases or as a host for less important activities (error reporting, printing, etc)
  //  val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
  //  val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
  //  if (phasesSet.exists(_.phaseName == "typer")) { // `scalac -help` doesn't instantiate standard phases
  //    def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
  //    val oldScs @ List(oldNamer, oldPackageobjects, oldTyper) = List(subcomponentNamed("namer"), subcomponentNamed("packageobjects"), subcomponentNamed("typer"))
  //    val newScs = List(analyzer.namerFactory, analyzer.packageObjects, analyzer.typerFactory)
  //    def hijackDescription(pt: SubComponent, sc: SubComponent) = phasesDescMap(sc) = phasesDescMap(pt) + " in paradise"
  //    oldScs zip newScs foreach { case (pt, sc) => hijackDescription(pt, sc) }
  //    phasesSet --= oldScs
  //    phasesSet ++= newScs
  //  }

  // replace Global.analyzer to customize namer and typer (step 3 of 3)
  // now let's take a look at what we couldn't replace during steps 1 and 2
  // here's what gets printed if add the following line to the standard Namer and Typer classes
  // if (!getClass.getName.startsWith("org.scalamacros.paradise")) println(getClass.getName)
  //
  //    scala.tools.nsc.Global$typer$
  //    scala.tools.nsc.typechecker.Implicits$ImplicitSearch
  //    ...
  //    scala.tools.nsc.transform.Erasure$Eraser
  //    ...
  //    scala.tools.nsc.typechecker.Namers$NormalNamer
  //    scala.tools.nsc.transform.Erasure$Eraser
  //    scala.tools.nsc.transform.Erasure$Eraser
  //    scala.tools.nsc.typechecker.Namers$NormalNamer
  //    ...
  //
  // duh, we're still not done. but the good news is that it doesn't matter for macro paradise:
  // 1) ImplicitSearch is easily worked around by overriding inferImplicit and allViewsFrom
  // 2) scala.tools.nsc.Global$typer$ is only used in later phases or as a host for less important activities (error reporting, printing, etc)
  // 3) Custom erasure typer and namers it spawns are also only used in later phases
  //
  // TODO: theoretically, points 2 and 3 can still be customizable
  // Global.typer can have itself set as a specialized subclass of scala.tools.nsc.Global$typer$
  // (it's possible because by now it's still null, as object initialization is lazy)
  // Erasure can be hijacked in the same way as we hijack namer, packageobjects and typer
  // however, for now this is all not essential, so I'm moving on
  ()

  val components = List[NscPluginComponent](TastyComponent, PluginComponent)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsAfter = List("parser")
    val phaseName = "tastygeneration"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        // do nothing else: everything's already hijacked
        ensureInitialized()
      }
    }

    var uninitialized = true
    def ensureInitialized() = {
      if (uninitialized) {
        uninitialized = false
      }
    }
  }
}