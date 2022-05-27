val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "typelevel",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-mtl"    % "1.2.1",
      "org.typelevel" %% "cats-effect" % "3.3.12"
    )
  )
