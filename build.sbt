import Dependencies._

val scala213 = "2.13.1"

val Versions = new {
  val circe = "0.13.0"
}

ThisBuild / pomIncludeRepository := { _ =>
  false
}
ThisBuild / publishConfiguration      := publishConfiguration.value.withOverwrite(true)
ThisBuild / publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/kolov/formi"),
    "scm:git@github.com:kolov/formi.git"
  )
)
ThisBuild / homepage := Some(url("https://github.com/kolov/formi"))

ThisBuild / developers := List(
  Developer(
    id = "kolov",
    name = "Assen Kolov",
    email = "assen.kolov@gmail.com",
    url = url("https://github.com/kolov")
  )
)

val basicSettings = Seq(
  scalaVersion              := scala213,
  organization              := "com.akolov",
  scalafmtOnCompile         := true,
  fork in Test              := true,
  parallelExecution in Test := true,
  libraryDependencies ++= Seq(
    "org.log4s" %% "log4s"         % "1.8.2",
    "ch.qos.logback"               % "logback-classic" % "1.2.3",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test",
    "org.scalamock" %% "scalamock" % "4.4.0" % "test"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-Ymacro-annotations"
  ),
//  publishTo := Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
)

val circeLib: Seq[ModuleID] = {
  def circe(name: String) =
    "io.circe" %% s"circe-$name" % Versions.circe

  Seq(
    circe("core"),
    circe("generic"),
    circe("parser"),
    circe("parser") % Test
  )
}

def module(moduleName: String) =
  Project(moduleName, file(moduleName))
    .settings(
      basicSettings ++ Seq(
        name               := s"formi-$moduleName",
        releaseTagName     := s"formi-$moduleName-${(version in ThisBuild).value}",
        crossScalaVersions := Seq(scala213)
      )
    )

lazy val core = module("core")
  .settings(
    libraryDependencies ++=
      Seq(
        "org.typelevel" %% "cats-free" % "2.1.1"
      )
  )
  .enablePlugins(JavaAppPackaging)

lazy val circe = module("circe")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= circeLib
  )
  .enablePlugins(JavaAppPackaging)

lazy val html = module("html")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= circeLib
  )
  .enablePlugins(JavaAppPackaging)

lazy val docs = project
  .in(file("project-docs")) // important: it must not be docs/
  .dependsOn(core, html)
  .enablePlugins(MdocPlugin)
  .settings(
    basicSettings ++ Seq(
      publish / skip := true
    ),
    mdocOut := new java.io.File(".")
  )

val curricula = (project in file("."))
  .aggregate(
    circe,
    core,
    html
  )
  .settings(
    name           := "formi",
    publish / skip := true
  )
