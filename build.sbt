name := "scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
val catsVersion = "2.2.0"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
  "org.typelevel" %% "cats-core" % catsVersion
)