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

  // install a pretty description for our plugin phase instead of empty string hardcoded for all plugins
  //  val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
  //  val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
  //  phasesDescMap(PluginComponent) = "let our powers combine"

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
  //  object genBCode2 extends {
  //    val global: Plugin.this.global.type = Plugin.this.global
  //    val runsAfter = List("dce")
  //    val runsRightAfter = None  
  //  } with GenBCode
  //  val genBCode2: scala.tools.nsc.Global$genBCode$ = (new { 
  //    override val global: Plugin.this.global.type = Plugin.this.global
  ////    override val runsAfter = List("dce")
  ////    override val runsRightAfter = None  
  //  } with scala.tools.nsc.Global$genBCode$(global) with GenBCode2)
  //  val genBCode2 = (new {
  //    val global: Plugin.this.global.type = Plugin.this.global
  //    val runsAfter = List("dce")
  //    val runsRightAfter = None  
  //  } with GenBCode2)
  //    object genBCode2 extends {
  //      override val global: Plugin.this.global.type = Plugin.this.global
  //      override val runsAfter = List("dce") 
  //    } with scala.tools.nsc.Global$genBCode$(global) with GenBCode2
  //      val genBCode2 = new {
  //    override val global: Plugin.this.global.type = Plugin.this.global
  //    //override val runsAfter: scala.collection.immutable.List[String] = List("dce")
  //  } with scala.tasty.internal.scalac.gencode.GenBCode2(global)
  //  val genBCode2 = new {
  //    override val global: Plugin.this.global.type = Plugin.this.global
  //    //override val runsAfter = List("dce")
  //  } with scala.tools.nsc.Global$genBCode$(global) with scala.tasty.internal.scalac.gencode.GenBCode2 //scala.tasty.internal.scalac.gencode.GenBCode2(global)

  //  abstract class NewAnalyzer extends Analyzer {
  //    def justCheck = println("!!!JUST CHECKING!!!")
  //  }

  //object testGenBCode extends scala.tools.nsc.Global$genBCode$(global)// with scala.tasty.internal.scalac.Plugin$genBCode2$

  //  val analyzer = classOf[Global].getDeclaredField("analyzer")
  //  println(classOf[Global].getDeclaredFields.toList filter (_.toString.contains("gen")))
  //  println
  //  println(classOf[scala.tools.nsc.Global$genBCode$].getDeclaredFields.toList)
  //  println
  //  println(classOf[scala.tools.nsc.Global$genBCode$].getDeclaredMethods.toList)
  //  println
  //  println(classOf[scala.tools.nsc.Global$genBCode$].getDeclaredClasses.toList)
  //  println(s"1: ${global.genBCode.toString()}")
  //  println
  //  println(s"2: ${global.genBCode.toString()}")

  //  val analyzer = new { val global: Plugin.this.global.type = Plugin.this.global } with NewAnalyzer
  //  val analyzerField = classOf[Global].getDeclaredField("analyzer")
  //  analyzerField.setAccessible(true)
  //  analyzerField.set(global, analyzer)

  //  val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
  //  genBCodeField.setAccessible(true)
  //  genBCodeField.set(global, genBCode2)

  //  trait NewErasure extends Erasure {
  //    class NewErasureTransformer(unit: global.CompilationUnit) extends ErasureTransformer(unit) {
  //      override def transform(tree: global.Tree): global.Tree = {
  //        println("!!! transform in erasure !!!")
  //        super.transform(tree)
  //      }
  //    }
  //    
  //    override def newTransformer(unit: global.CompilationUnit) = {
  //      println("!!! newTransformer for erasure !!!")
  //      new NewErasureTransformer(unit)
  //    }
  //  }
  //
  //  class WrapperForErasure {
  //    object erasure extends {
  //      override val global: Plugin.this.global.type = Plugin.this.global
  //      //override val runsAfter = List("dce")
  //    } with scala.tools.nsc.Global$erasure$(global) with NewErasure
  //  }
  //
  //  val wrapper = new WrapperForErasure()
  //  val erasureField = classOf[WrapperForErasure].getDeclaredField("erasure$module")
  //  erasureField.setAccessible(true)
  //  val value = erasureField.get(wrapper)
  //  val erasure2Field = classOf[Global].getDeclaredField("genBCode$module")
  //  erasure2Field.setAccessible(true)
  //  erasure2Field.set(global, value)
  
//  global.genBCode

  //  class WrapperForGenBCode2 {
  //    object genBCode extends {
  //      override val global: Plugin.this.global.type = Plugin.this.global
  //      val runsAfter = List("dce")
  //      val runsRightAfter = None
  //    } with scala.tasty.internal.scalac.gencode.GenBCode2
  //  }

    /*** wrapper class ***/
  class WrapperForGenBCode2 {
    object genBCode extends {
      override val global: Plugin.this.global.type = Plugin.this.global
    } with scala.tasty.internal.scalac.gencode.GenBCode2(global) //with scala.tools.nsc.Global$genBCode$(global) with scala.tasty.internal.scalac.gencode.GenBCode2
  }
  
  val wrapper = new WrapperForGenBCode2()
  println(s"wrapper.genBCode (wrapper.genBCode): ${wrapper.genBCode}")
  val newGenBCodeField = classOf[WrapperForGenBCode2].getDeclaredField("genBCode$module")
  newGenBCodeField.setAccessible(true)
  //to init object
  val value = newGenBCodeField.get(wrapper)
  println(s"VAL (value): $value")
  println(s"value.isGenBCode2: ${value.isInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2]}")
  println
////  print(" (before setting) "); value.asInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2].myTest; 
  /*** wrapper class ***/
  
  
  val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
  genBCodeField.setAccessible(true)
  
  //read value of genBCodeField
  val v2 = genBCodeField.get(global)
  println(s"VAL (v2 - during init): $v2")
  println(s"v2.isGenBCode2: ${v2.isInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2]}")
  println(s"value == v2: ${value == v2}")
  
  println(s"declared fields: ${classOf[scala.tools.nsc.Global$genBCode$].getDeclaredFields.toList}")
  println
  println(s"declared methods: ${classOf[scala.tools.nsc.Global$genBCode$].getDeclaredMethods.toList}")
  println
  println(s"fields: ${classOf[scala.tools.nsc.Global$genBCode$].getFields.toList}")
  println
  println(s"genBCode(original):")
  println(s"superclass: ${classOf[scala.tools.nsc.Global$genBCode$].getSuperclass}")
  println
  println(s"interfaces: ${classOf[scala.tools.nsc.Global$genBCode$].getInterfaces.toList}")
  println
  println(s"generic interfaces: ${classOf[scala.tools.nsc.Global$genBCode$].getGenericInterfaces.toList}")
  println
  println(s"generic interfaces: ${classOf[scala.tools.nsc.Global$genBCode$].getGenericSuperclass}")
  println
  println(s"genBCode2(:")
  println(s"superclass: ${value.getClass.getSuperclass}")
  println
  println(s"interfaces: ${value.getClass.getInterfaces.toList}")
  println
  println(s"generic interfaces: ${value.getClass.getGenericInterfaces.toList}")
  println
  println(s"generic interfaces: ${value.getClass.getGenericSuperclass}")
  println
  
  
  /*** replace ***/
  genBCodeField.set(global, value)
  //  val r = genBCodeField.get(global)
  /*** replace ***/

  // replace Global.analyzer to customize namer and typer (step 2 of 3)
  // luckily for us compiler plugins are instantiated quite early
  // so by now internal phases have only left a trace in phasesSet and in phasesDescMap
  // also up until now noone has really used the standard analyzer, so we're almost all set
  // except for the standard `object typer extends analyzer.Typer(<some default context>)`
  // that is a member of Global and hence has been pre-initialized now
  // good news is that it's only used in later phases or as a host for less important activities (error reporting, printing, etc)
  val phasesSetMapGetter = classOf[Global].getDeclaredMethod("phasesSet")
  val phasesDescMapGetter = classOf[Global].getDeclaredMethod("phasesDescMap")
  val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
  phasesDescMap(PluginComponent) = "let our powers combine"
  val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
  if (phasesSet.exists(_.phaseName.contains("jvm"))) { // `scalac -help` doesn't instantiate standard phases
    def subcomponentNamed(name: String) = phasesSet.find(_.phaseName == name).head
    val oldScs @ List(oldJVM) = List(subcomponentNamed("jvm"))
    println
    println(s"oldScs: $oldScs")
    println
    val newScs = List(value.asInstanceOf[SubComponent])
    def hijackDescription(pt: SubComponent, sc: SubComponent) = phasesDescMap(sc) = phasesDescMap(pt) + " in paradise"
    oldScs zip newScs foreach { case (pt, sc) => hijackDescription(pt, sc) }
    phasesSet --= oldScs
    phasesSet ++= newScs
    println("!!! YES it's here !!!")
  } else println("!!! NO it's not here !!!")
  println(s"phasesSet: ${phasesSet}")
  
  
  
  
  
//  println(s"VAL (r): $r")
//  global.genBCode
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

  val components = List[NscPluginComponent](/*TastyComponent, */ PluginComponent, PluginComponentAfterJVM)

  object PluginComponent extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsBefore = List("jvm")
    override val runsAfter = List("dce")
    val phaseName = "tastygeneration"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("!!! Phase is running !!!")
        // do nothing else: everything's already hijacked
        //        println(global.genBCode.toString())
        //        global.genBCode.asInstanceOf[GenBCode2].myTest
        //        global.analyzer.asInstanceOf[NewAnalyzer].justCheck
        //        val genBCode2 = new {
        //          override val global: Plugin.this.global.type = Plugin.this.global
        //          //override val runsAfter = List("dce")
        //        } with scala.tools.nsc.Global$genBCode$(global) with scala.tasty.internal.scalac.gencode.GenBCode2
        //        class WrapperForGenBCode2 {
        //          object genBCode extends {
        //            override val global: Plugin.this.global.type = Plugin.this.global
        //            //override val runsAfter = List("dce")
        //          } /*with scala.tools.nsc.Global$genBCode$(global)*/ with scala.tasty.internal.scalac.gencode.GenBCode2
        //        }
        //        class WrapperForGenBCode2 {
        //          object genBCode extends {
        //            override val global: Plugin.this.global.type = Plugin.this.global
        //            val runsAfter = List("dce")
        //            val runsRightAfter = None  
        //          } with scala.tasty.internal.scalac.gencode.GenBCode2
        //        }
        //        class WrapperForGenBCode2 {
        //          val genBbbCode2 = new {
        //            override val global: Plugin.this.global.type = Plugin.this.global
        //            //override val runsAfter = List("dce")
        //          } with scala.tools.nsc.Global$genBCode$(global) //with scala.tasty.internal.scalac.gencode.GenBCode2
        //        }        

        //        val wrapper = new WrapperForGenBCode2()
        //        println(s"wrapper.genBCode: ${wrapper.genBCode}")
        //        val newGenBCodeField = classOf[WrapperForGenBCode2].getDeclaredField("genBCode$module")
        //        newGenBCodeField.setAccessible(true)
        //        //to init object
        //        val value = newGenBCodeField.get(wrapper)
        //        println(s"value: $value")
        //        value.asInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2].myTest; print(" (before setting)")
        //        val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
        //        genBCodeField.setAccessible(true)
        //        genBCodeField.set(global, value)
        //          global.erasure
        //        global.genBCode
        //          global.genBCode.asInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2].myTest
        //global.genBCode
        val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
        genBCodeField.setAccessible(true)
        
        //read value of genBCodeField
        val v2 = genBCodeField.get(global)
        println(s"VAL (v2 - before jvm): $v2")
        println(s"v2.isGenBCode2: ${v2.isInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2]}")
        println(v2.asInstanceOf[GenBCode].phaseName)
        println(v2.asInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2].phaseName)

        v2.asInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2].myTest
        println(s"value == v2: ${value == v2}")
        println(s"settings.isBCodeActive ${settings.isBCodeActive}")
        println
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
  object PluginComponentAfterJVM extends NscPluginComponent {
    val global = Plugin.this.global
    import global._

    override val runsAfter = List("jvm")
    val phaseName = "tastygenerationafterjvm"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        println("!!! Phase after JVM is running !!!")

        val genBCodeField = classOf[Global].getDeclaredField("genBCode$module")
        genBCodeField.setAccessible(true)

        //read value of genBCodeField
        val v2 = genBCodeField.get(global)
        println(s"VAL (v2 - after jvm): $v2")
        println(s"v2.isGenBCode2: ${v2.isInstanceOf[scala.tasty.internal.scalac.gencode.GenBCode2]}")
        println(s"value == v2: ${value == v2}")
        println
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