import sbt._
import Keys._

object ScalaHostBuild extends Build {
  import Dependencies._
  import Settings._

  lazy val commonDependencies = Seq(
    libraryDependencies <++= (scalaVersion)(sv => Seq(
      reflect(sv) % "provided",
      compiler(sv) % "provided"
    ))
  )

  lazy val root = Project(
    id = "root",
    base = file("root"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      dontPackage,
      usePlugin(plugin)
    )
  ) aggregate (plugin, sandbox, tests) dependsOn (plugin)

  lazy val plugin = Project(
    id   = "tasty",
    base = file("plugin"),
    settings = publishableSettings ++ commonDependencies ++ mergeDependencies
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      usePlugin(plugin)
    )
  ) dependsOn(plugin)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      usePlugin(plugin),
      libraryDependencies ++= Seq(scalatest, scalacheck),
      dontPackage
    ) ++ exposeClasspaths("tests")
  ) dependsOn (plugin)
}