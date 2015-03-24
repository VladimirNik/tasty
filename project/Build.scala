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
      usePlugin(plugin),
      unmanagedJars in Compile <++= baseDirectory map { base =>
        val dottyJar = file("/home/vova/scala-projects/dotty/dotty/bin/../target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar")
        val dottyTestJar = file("/home/vova/scala-projects/dotty/dotty/bin/../target/scala-2.11/dotty_2.11-0.1-SNAPSHOT-tests.jar")
        val dottyJars = dottyJar +++ dottyTestJar
        //val baseDirectories = (base / "libA") +++ (base / "b" / "lib") +++ (base / "libC")
        //val customJars = (baseDirectories ** "*.jar") +++ (base / "d" / "my.jar")
        dottyJars.classpath
      }
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