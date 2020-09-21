name := "barrage"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.3"

inThisBuild(Seq(
  scalacOptions += "-Ymacro-annotations",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
))

scalacOptions -= "-Xfatal-warnings"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "io.estatico" %% "newtype" % "0.4.4",
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "com.github.julien-truffaut" %% "monocle-core" % "2.1.0",
  "com.github.julien-truffaut" %% "monocle-macro" % "2.1.0",
  "com.github.julien-truffaut" %% "monocle-unsafe" % "2.1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.0" % Test
)
