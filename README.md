# contra-tracer (Scala)

A typed, composable **contravariant tracer** for Scala 3, built on [Cats](https://typelevel.org/cats/).

A component that "logs" stays completely unaware of the logging backend. It only emits **typed
events** (`tracer.traceWith(event)`); the eventual caller supplies the functions that format and
render those events within some effect monad `M`. This is a Scala 3 / Cats port of Alexander
Vieth's Haskell [`contra-tracer`](https://github.com/avieth/contra-tracer).

## Why an arrow, not just `A => M[Unit]`

A `ContraTracer[M, A]` is morally `A => M[Unit]`, but it is built over the `TracerA` arrow so the
emitting / non-emitting structure is preserved. That structure buys two things a plain function
can't:

- **Laziness / free silencing.** A squelching (no-op) tracer — e.g. `Monoid.empty` /
  `ContraTracer.nullTracer` — skips all upstream `contramapM` / predicate / payload-construction
  work. The argument is never forced, and a squelched `traceWith` compiles to a constant `unit`, so
  it is allocation-free on the JVM. With a bare `A => M[Unit]` the predicate must be forced no matter
  what, because you can't know a priori whether the function will emit.
- **Composition.** Combine sinks with `|+|` (`Semigroup` / `Monoid`), narrow the input type with
  `contramap`, gate emission with `squelchUnless` / `traceMaybe`, or swap the effect with a natural
  transformation via `natTracer`.

## Install

```scala
libraryDependencies += "org.cardano-hydrozoa" %% "contra-tracer" % "0.1.0-SNAPSHOT"
```

(Not yet published to a public repository — `sbt publishLocal` for now.)

## Usage

```scala
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer

sealed trait MyEvent
object MyEvent:
    final case class Started(at: Long)      extends MyEvent
    final case class Stalled(reason: String) extends MyEvent

// A sink is any `A => M[Unit]`.
val console: ContraTracer[IO, String] =
    ContraTracer.emit[IO, String](line => IO.println(line))

// `contramap` a backend-agnostic sink down to your typed event.
val tracer: ContraTracer[IO, MyEvent] =
    console.contramap {
        case MyEvent.Started(at)  => s"started at $at"
        case MyEvent.Stalled(why) => s"stalled: $why"
    }

tracer.traceWith(MyEvent.Started(0L))   // IO[Unit]
```

Keep `M` polymorphic (`def step[F[_]: Monad](tracer: ContraTracer[F, MyEvent]) = ...`) so the same
code runs under `IO`, `cats.Id` (pure/synchronous callers), or any other monad. Silence a leg with
`ContraTracer.nullTracer` and the whole upstream chain is skipped for free.

### Composing sinks

```scala
val captured = scala.collection.mutable.ListBuffer.empty[MyEvent]
val capture  = ContraTracer.emit[IO, MyEvent](e => IO(captured += e).void)

// Runs both, same input: render to the console AND capture for a test assertion.
val both: ContraTracer[IO, MyEvent] = tracer |+| capture
```

See `src/test/scala/hydrozoa/lib/logging/ContraTracerDemo.scala` for a worked tour of `contramap`,
`|+|`, `traceMaybe` / `squelchUnless`, `natTracer`, and the free-silencing property.

## Layout

- `ContraTracer.scala` — the user-facing tracer: `traceWith`, `contramap`, `traceMaybe`,
  `squelchUnless(M)`, `contramapM`, `natTracer`, and the `Semigroup` / `Monoid` / `Contravariant`
  instances.
- `TracerA.scala` — the underlying tagged Kleisli arrow (`Emitting` / `Squelching`) and its `Arrow`
  / `ArrowChoice` instances, plus the compile step that makes squelched traces allocation-free.

The only runtime dependency is `cats-core`. The test suite additionally uses `cats-effect` and
`scalatest`.

## Building

```bash
sbt compile
sbt test
sbt scalafmtCheckAll   # formatting
```

Requires sbt 2 (see `project/build.properties`).

## License & attribution

Licensed under the [Apache License 2.0](LICENSE).

This is a derivative work — a Scala 3 / Cats port of the Haskell
[`contra-tracer`](https://github.com/avieth/contra-tracer) by Alexander Vieth, itself Apache-2.0
(Copyright 2019-2021 Input Output (Hong Kong) Ltd., Well-Typed LLP, and Alexander Vieth). See
[NOTICE](NOTICE) for the full attribution.
