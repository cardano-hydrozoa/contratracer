ThisBuild / scalaVersion := "3.3.7"
// Published via JitPack, which serves artifacts under `com.github.<org>` and resolves the
// requested version against a git tag (trying both `X` and `vX`). So the groupId must be
// `com.github.cardano-hydrozoa` and `version` must match the release tag — to cut `v0.2.0`, set
// `version := "0.2.0"` here, commit, then tag `v0.2.0`. JitPack re-serves the built
// `contratracer_3` jar under the repo name (suffix stripped), so consumers use a single `%`:
//   "com.github.cardano-hydrozoa" % "contratracer" % "0.2.0"   (+ the JitPack resolver)
ThisBuild / organization := "com.github.cardano-hydrozoa"
ThisBuild / version := "0.2.0"

// The core library, published as `contratracer`. It aggregates the modules, so a command run at
// the root, JitPack's `publishM2` included, runs in every module too.
lazy val root: Project = (project in file("."))
    .aggregate(LocalProject("slf4j"))
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
        "org.scalatest" %% "scalatest" % "3.2.19" % Test,
        // Test-only: the law suites (cats' Arrow/Contravariant/Monoid suites via discipline).
        "org.typelevel" %% "cats-laws" % "2.13.0" % Test,
        "org.typelevel" %% "discipline-scalatest" % "2.3.0" % Test
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

// SLF4J sinks keyed by level. Core keeps cats-core as its only dependency; this module adds
// slf4j-api and the cats-effect kernel (for `Sync`), and no logging backend.
lazy val slf4j: Project = (project in file("contratracer-slf4j"))
    .dependsOn(root)
    .settings(
      name := "contratracer-slf4j",
      description := "SLF4J sinks for contratracer that ask the logger before an event is built.",
      licenses := (root / licenses).value,
      libraryDependencies ++= Seq(
        "org.slf4j" % "slf4j-api" % "2.0.17",
        "org.typelevel" %% "cats-effect-kernel" % "3.6.3",
        "org.typelevel" %% "cats-effect" % "3.6.3" % Test,
        "ch.qos.logback" % "logback-classic" % "1.5.18" % Test,
        "org.scalatest" %% "scalatest" % "3.2.19" % Test
      ),
      scalacOptions ++= (root / scalacOptions).value
    )
