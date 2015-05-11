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
    override val runsRightAfter = Some("typer")
    val phaseName = "tasty"
    override def description = "pickle tasty trees"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        val tree = unit.body

        if (!unit.isJava) {
          val tree = unit.body
          val picklers = new {
            val global: self.global.type = self.global
          } with TreePicklers
          val pickler = new picklers.TastyPickler            
          val treePkl = new picklers.TreePickler(pickler)
          treePkl.pickle(tree :: Nil)
          
          //add option for pickling tesing (if option - test - option pass to sbt tests subproject)
          val pickledInfo = treePkl.logInfo
          testSame(pickledInfo, unit)
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