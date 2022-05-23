val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "cats-effect-3",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-language:postfixOps"),
    libraryDependencies ++= Seq("org.typelevel" %% "cats-effect" % "3.3.12")
  )
