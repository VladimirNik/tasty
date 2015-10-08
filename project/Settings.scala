import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._

object Settings {
  lazy val languageVersion = "2.11.7"
  lazy val tastyVersion = "0.1.0-SNAPSHOT"

  lazy val sharedSettings: Seq[sbt.Def.Setting[_]] = Defaults.defaultSettings ++ Seq(
    scalaVersion := languageVersion,
    scalaHome := Some(file("/home/vova/scala-projects/backendPlugin/scala/build/pack")),
    crossVersion := CrossVersion.full,
    version := tastyVersion,
    organization := "org.tasty",
    description := "Tasty for Scala",
    //resolvers += Resolver.sonatypeRepo("snapshots"),
    //resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Ybackend:GenBCode"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    traceLevel := 0,
    // scalaHome := {
    //  val scalaHome = System.getProperty("tasty.scala.home")
    //  if (scalaHome != null) {
    //    println(s"Going for custom scala home at $scalaHome")
    //    Some(file(scalaHome))
    //  } else None
    // },
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false
  )

  lazy val mergeDependencies: Seq[sbt.Def.Setting[_]] = assemblySettings ++ Seq(
    test in assembly := {},
    logLevel in assembly := Level.Error,
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) },
    Keys.`package` in Compile := {
      val slimJar = (Keys.`package` in Compile).value
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      slimJar
    },
    packagedArtifact in Compile in packageBin := {
      val temp = (packagedArtifact in Compile in packageBin).value
      val (art, slimJar) = temp
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      (art, slimJar)
    }
  )

  lazy val publishableSettings: Seq[sbt.Def.Setting[_]] = sharedSettings ++ Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false
  )

  lazy val dontPackage = packagedArtifacts := Map.empty

  // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
  // add plugin timestamp to compiler options to trigger recompile of
  // main after editing the plugin. (Otherwise a 'clean' is needed.)
  def usePlugin(plugin: ProjectReference) =
    scalacOptions <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("sbt.paths.plugin.jar", jar.getAbsolutePath)
      Seq("-Xplugin:" + jar.getAbsolutePath, "-Jdummy=" + jar.lastModified)
    }

  def exposeClasspaths(projectName: String) = Seq(
    fullClasspath in Test := {
      val defaultValue = (fullClasspath in Test).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      val scalaLibrary = classpath.map(_.toString).find(_.contains("scala-library")).get
      System.setProperty("sbt.paths.scala-library.jar", scalaLibrary)
      System.setProperty("sbt.paths.tests.classpath", classpath.mkString(java.io.File.pathSeparatorChar.toString))
      defaultValue
    },
    resourceDirectory in Test := {
      val defaultValue = (resourceDirectory in Test).value
      System.setProperty("sbt.paths.tests.resources", defaultValue.getAbsolutePath)
      defaultValue
    }
  )
}