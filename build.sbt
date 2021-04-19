scalaVersion := "2.13.3"

name := "scala-bootcamp-homework"
version := "1.0"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations",
)

val circeVersion = "0.13.0"
val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"
val log4CatsVersion = "1.1.1"
val http4sVersion = "0.21.22"
val epimetheusVersion = "0.4.2"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % "0.3.6",
  "io.chrisdavenport" %% "epimetheus-http4s" % epimetheusVersion,

  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",

  "org.typelevel" %% "cats-core" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "org.typelevel" %% "cats-effect" % catsEffectVersion,

  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
