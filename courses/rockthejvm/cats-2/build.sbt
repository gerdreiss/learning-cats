val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "cats-2",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0"
    )
  )
