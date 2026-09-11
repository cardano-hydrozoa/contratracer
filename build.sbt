ThisBuild / scalaVersion := "3.3.7"
// Published via JitPack, which serves artifacts under `com.github.<org>` and resolves the
// requested version against a git tag (trying both `X` and `vX`). So the groupId must be
// `com.github.cardano-hydrozoa` and `version` must match the release tag — to cut `v0.1.1`, set
// `version := "0.1.1"` here, commit, then tag `v0.1.1`. JitPack re-serves the built
// `contratracer_3` jar under the repo name (suffix stripped), so consumers use a single `%`:
//   "com.github.cardano-hydrozoa" % "contratracer" % "0.1.1"   (+ the JitPack resolver)
ThisBuild / organization := "com.github.cardano-hydrozoa"
ThisBuild / version := "0.1.1"

lazy val root = (project in file("."))
    .settings(
      // Matches the GitHub repo name so the JitPack coordinate reads `…:contratracer:…`.
      name := "contratracer",
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
