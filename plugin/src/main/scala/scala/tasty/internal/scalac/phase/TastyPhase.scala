package scala.tasty
package internal.scalac
package phase

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.reflect.io.AbstractFile
import scala.tasty.internal.scalac.pickler.core.TreePicklers
import scala.tasty.internal.scalac.util.TastyUtils
import scala.tasty.internal.scalac.util.TastyGenUtils

trait TastyPhase {
  self =>

  val global: Global
 
  val picklersInstance = new {
    val global: self.global.type = self.global
  } with TreePicklers

  import scala.collection.mutable.{ Map => MMap }
  private var picklers: MMap[global.CompilationUnit, MMap[global.ClassSymbol, picklersInstance.TastyPickler]] = MMap()

  def addPickler(unit: global.CompilationUnit, classSymbol: global.ClassSymbol, pickler: picklersInstance.TastyPickler) =
    picklers get unit match {
      case Some(picklersMap) => picklersMap += (classSymbol -> pickler)
      case None              => picklers += (unit -> MMap(classSymbol -> pickler))
    }

  def findPickler(unit: global.CompilationUnit, classSymbol: global.ClassSymbol): Option[picklersInstance.TastyPickler] =
    picklers(unit) get (classSymbol)

  object TastyComponent extends {
    val global: self.global.type = self.global
  } with NscPluginComponent with TastyGenUtils {

    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
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
            val pickler = new picklersInstance.TastyPickler
            addPickler(unit, cls, pickler)
            val treePkl = new picklersInstance.TreePickler(pickler)
            treePkl.pickle(tree :: Nil)

            //add option for pickling tesing (if option - test - option pass to sbt tests subproject)
            //val pickledInfo = treePkl.logInfo
            //testSame(pickledInfo, unit)
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