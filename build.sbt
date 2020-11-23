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
val http4sVersion = "0.21.7"
val jakartaMailVersion = "2.0.0"
val log4CatsVersion = "1.1.1"
val redisClientVersion = "3.30"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "jakarta.mail" % "jakarta.mail-api" % jakartaMailVersion,
  "com.sun.mail" % "jakarta.mail" % jakartaMailVersion,
  "net.debasishg" %% "redisclient" % redisClientVersion,
  "org.slf4j" % "slf4j-nop" % "1.7.30",
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
  "com.codecommit" %% "cats-effect-testing-scalatest" % "0.4.1" % Test
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)