package scala.tasty
package internal
package scalac
package phase

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.reflect.io.AbstractFile
//import scala.tasty.internal.scalac.pickler.core.TreePicklers
//import scala.tasty.internal.scalac.util.TastyUtils
//import scala.tasty.internal.scalac.util.TastyGenUtils

trait TastyPhase extends TastyPhaseUtils {
  self =>

  val global: Global
 
  val apiInstance = new PicklerAPI(global)
  
//  val picklersInstance = new {
//    val global: self.global.type = self.global
//  } with TreePicklers

  import scala.collection.mutable.{ Map => MMap }
  private var picklers: MMap[global.ClassSymbol, apiInstance.TastyPickler] = MMap()

  def addPickler(classSymbol: global.ClassSymbol, pickler: apiInstance.TastyPickler) =
    picklers += (classSymbol -> pickler)

  def findPickler(classSymbol: global.ClassSymbol): Option[apiInstance.TastyPickler] =
    picklers get (classSymbol)

  object TastyComponent extends {
    val global: self.global.type = self.global
  } with NscPluginComponent /*with TastyGenUtils*/ {

    override val runsAfter = List("superaccessors")
    override val runsRightAfter = Some("superaccessors")
    val phaseName = "tasty"
    override def description = "pickle tasty trees"
    
    import global._
    
    final val TASTYATTR = "TASTY"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {

      private val beforePickling = new scala.collection.mutable.HashMap[ClassSymbol, String]

      /** Drop any elements of this list that are linked module classes of other elements in the list */
      private def dropCompanionModuleClasses(clss: List[ClassSymbol]): List[ClassSymbol] = {
        val companionModuleClasses =
          clss.filterNot(_.isModule).map(_.linkedClassOfClass) /*.filterNot(_.isAbsent)*/
        clss.filterNot(companionModuleClasses.contains)
      }

      override def apply(unit: CompilationUnit): Unit = {
        val tree = unit.body

        if (!unit.isJava) {
          for {
            cls <- dropCompanionModuleClasses(topLevelClasses(unit.body))
            tree <- sliceTopLevel(unit.body, cls)
          } {
            val tTree = apiInstance.convertTree(tree.asInstanceOf[apiInstance.g.Tree])
            val pickler = new apiInstance.TastyPickler
            addPickler(cls, pickler)
            val treePkl = new apiInstance.TreePickler(pickler)
            val emptyContext = new apiInstance.Contexts.Context{}
            treePkl.pickle(tTree :: Nil)(emptyContext)

            pickler.addrOfTree = treePkl.buf.addrOfTree
            pickler.addrOfSym = treePkl.addrOfSym
            if (tTree.pos.exists) {
              println(s"Pickle positions for $unit")
              println(s"pos: ${tree.pos}")
              println
              new apiInstance.PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tTree :: Nil, tTree.pos)(emptyContext)
            } else {
              println("No positions exist for pickling")
            }
            //add option for pickling testing (if option - test - option pass to sbt tests subproject)
            val pickledInfo = treePkl.logInfo
            generateTestFile(s"/home/vova/tasty-logs/${cls.name + ".tasty"}", pickledInfo)
            //testSame(pickledInfo, unit)

            analyzer.addBackendPlugin(new TastyBackendPlugin)
          }
        }
      }

      class TastyBackendPlugin extends analyzer.BackendPlugin {
    	import analyzer.ClassAttr
        override def isActive(): Boolean = true
        override def pluginsCustomAttributes(sym: ClassSymbol): List[ClassAttr] = {
            findPickler(sym) match {
              case Some(pickler) =>
                val binary = pickler.assembleParts()
                println(s"cls: $sym")
                println(s"binary.length: ${binary.length}\n")
                List(new ClassAttr(TastyComponent.TASTYATTR, binary))
              case _ => Nil
            }
        }
      }
    }

    import scala.io.Source
    import java.io.File

    private def testSame(pickledInScala: String, unit: CompilationUnit) = {
      var errorDuringFileReading = false
      def loadPickledPattern(file: File): String = {
        val absPath = file.getAbsolutePath.dropRight(".scala".length()).replaceFirst("sandbox", "tests")
        val testFilePath = absPath + "tasty"
        generateTestFile(testFilePath, pickledInScala)
        try {
          import scala.reflect.internal.Chars.LF
          Source.fromFile(absPath).getLines.mkString(s"${LF}").trim().stripLineEnd
        } catch {
          case ex: Exception =>
            errorDuringFileReading = true
            s"file ${file.getName} can not be read"
        }
      }
      val pickledInDotty = loadPickledPattern(unit.source.file.file)
      if (pickledInScala != pickledInDotty) {
        if (errorDuringFileReading) warning(s"$pickledInDotty")
        else warning(s"pickling difference for $unit")
      } else {
        inform(s"pickling is correct for $unit")
      }
    }

    def generateTestFile(path: String, logInfo: String) = {
      import java.io.FileWriter
      try {
        val logFile = new File(path);
        val logFileWriter = new FileWriter(logFile, false); // true to append
        logFileWriter.write(logInfo);
        logFileWriter.close();
      } catch {
        case ex: Exception =>
          warning(s"test file: $path can not be generated")
      }
    }
  }
}