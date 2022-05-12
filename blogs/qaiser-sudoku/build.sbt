val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "qaiser-sudoku",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.11",
      "co.fs2"        %% "fs2-core"    % "3.2.7"
    )
  )
