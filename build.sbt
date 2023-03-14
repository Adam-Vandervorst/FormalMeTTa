ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "FormalMeTTa",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M7" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0"
  )
