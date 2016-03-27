import java.nio.file.{Paths, Files}
import java.nio.file.StandardCopyOption._

import sbt.Keys._

assembly := {
  val assemblyValue = assembly.value
  Files.copy(assemblyValue.toPath, Paths.get(".").resolve(assemblyValue.toPath.getFileName), REPLACE_EXISTING)
  assemblyValue
}

lazy val versionHttp4s = "0.12.4"

lazy val root = (project in file(".")).
  settings(
    name := "edu.gatech.dt87.scalaverse",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := "2.11.4",
    libraryDependencies ++= Seq(
      "com.gilt" %% "handlebars-scala" % "2.0.1",
      "com.github.julien-truffaut" %% "monocle-core" % "0.5.1",
      "com.github.julien-truffaut" %% "monocle-generic" % "0.5.1",
      "com.github.julien-truffaut" %% "monocle-macro" % "0.5.1",
      "org.http4s" %% "http4s-dsl" % versionHttp4s, // to use the core dsl
      "org.http4s" %% "http4s-tomcat" % versionHttp4s, // to use the jetty servlet backend
      "org.scalacheck" %% "scalacheck" % "1.12.0" % "test",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test"),
    mainClass.in(assembly) := Some("edu.gatech.dt87.multiverse.ui.Main"),
    assemblyJarName.in(assembly) := "multiverse.jar"
  )