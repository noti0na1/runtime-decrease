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

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "sourcecode" % "0.3.1",
      "io.github.martinhh" %% "scalacheck-derived" % "0.4.2" % Test,
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
    ),
    
    Test / testOptions += Tests.Argument(
      TestFrameworks.ScalaCheck,
      "-maxSize",
      "10",
      // "-workers",
      // "1",
      "-verbosity",
      "1"
    )
  )
