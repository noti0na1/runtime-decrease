val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "runtime-decrease",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-source:future"
    ),

    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
  )
