ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "org.cardano-hydrozoa"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
    .settings(
      name := "contra-tracer",
      description :=
          "A typed, composable contravariant tracer for Scala 3 / Cats, ported from " +
              "Alexander Vieth's Haskell contra-tracer.",
      licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.13.0",
        // Test-only: the demo suite drives tracers in IO to show the laziness/composition laws.
        "org.typelevel" %% "cats-effect" % "3.6.3" % Test,
        "org.scalatest" %% "scalatest" % "3.2.19" % Test
      ),
      scalacOptions ++= Seq(
        "-feature",
        "-deprecation",
        "-unchecked",
        "-language:implicitConversions",
        "-Wvalue-discard",
        "-Wunused:all",
        "-Wall",
        "-Wconf:msg=interpolation uses toString:s",
        "-Yretain-trees"
      )
    )
