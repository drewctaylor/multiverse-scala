import java.nio.file.{Paths, Files}
import java.nio.file.StandardCopyOption._

import sbt.Keys._

sbtassembly.AssemblyKeys.assembly := {
    val assemblyValue = sbtassembly.AssemblyKeys.assembly.value
    Files.copy(assemblyValue.toPath, Paths.get(".").resolve(assemblyValue.toPath.getFileName), REPLACE_EXISTING)
    assemblyValue
}

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
            "org.scalacheck" %% "scalacheck" % "1.12.0" % "test",
            "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"),
        mainClass.in(sbtassembly.AssemblyKeys.assembly) := Some("edu.gatech.dt87.multiverse.ui.Main"),
        sbtassembly.AssemblyKeys.assemblyJarName.in(sbtassembly.AssemblyKeys.assembly) := "multiverse.jar"
    )

