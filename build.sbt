import Dependencies._

val scala213 = "2.13.1"

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
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)
)

val circeLib: Seq[ModuleID] = {
  def circe(name: String, version: String = "0.12.3") =
    "io.circe" %% s"circe-$name" % version

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

lazy val docs = project
  .in(file("project-docs")) // important: it must not be docs/
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
  .settings(
    basicSettings ++ Seq(
      crossScalaVersions := Nil,
      publish / skip     := true
    ),
    mdocOut := new java.io.File(".")
  )

val curricula = (project in file("."))
  .aggregate(
    circe,
    core
  )
  .settings(
    name    := "formi",
    publish := {}
  )
