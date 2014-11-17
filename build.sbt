name := "edu.gatech.dt87.scalaverse"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
    "com.gilt" %% "handlebars-scala" % "2.0.1",
    "com.github.julien-truffaut" %% "monocle-core" % "0.5.1",
    "com.github.julien-truffaut" %% "monocle-generic" % "0.5.1",
    "com.github.julien-truffaut" %% "monocle-macro" % "0.5.1")

assemblyJarName in assembly := "multiverse.jar"