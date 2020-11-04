name := "scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

val catsVersion = "2.2.0"
val circeVersion = "0.13.0"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
  "com.codecommit" %% "cats-effect-testing-scalatest" % "0.4.1" % Test
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)