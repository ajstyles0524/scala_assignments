ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "scala_assignments"
  )
