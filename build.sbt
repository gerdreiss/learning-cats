organization := "com.github.gerdreiss"
name := "learning-cats"
version := "1.0.0"
scalaVersion := "2.12.15"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")


enablePlugins(JavaAppPackaging)

dockerBaseImage := "openjdk:8-jre-alpine"
packageName in Docker := "dockerised-learning-cats"

import com.typesafe.sbt.packager.docker._
dockerCommands ++= Seq(
  Cmd("USER", "root"),
  ExecCmd("RUN", "apk", "add", "--no-cache", "bash")
)