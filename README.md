# contra-tracer (Scala)

A typed, composable **contravariant tracer** for Scala 3, built on
[Cats](https://typelevel.org/cats/): a port of Alexander Vieth's Haskell
[`contra-tracer`](https://github.com/avieth/contra-tracer).

A component that logs takes a `ContraTracer[M, Event]` and calls `tracer.traceWith(event)` with its
own typed events. It never sees the logging backend. Whoever builds the component decides how each
event is formatted and where it goes.

## Install

Published via [JitPack](https://jitpack.io/#cardano-hydrozoa/contratracer). Add the resolver and
depend on a release tag:

```scala
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.cardano-hydrozoa" % "contratracer" % "0.2.0"
```

Note the **single `%`**: JitPack re-serves the built `contratracer_3` artifact under the repo name
with the Scala suffix stripped (`contratracer`), so `%%` would not resolve. The published jar is a
Scala 3 artifact regardless. You can also track a branch (`"main-SNAPSHOT"`) or a specific commit
hash in place of the tag.

The only runtime dependency is `cats-core`.

## Usage

### Define events

Give each component its own event type, as a sealed trait or an enum. An event carries values the
component already holds; turning them into text is the format's job.

```scala
enum FetcherEvent:
    case Started(url: String)
    case Fetched(url: String, bytes: Array[Byte])
    case Failed(url: String, error: Throwable)
```

### Wire with `contramap`

A sink is a tracer over the backend's own type. `contramap` adapts it to a component's events with a
format function. The library has no levels and no backend; in this example, `Level`, `LogLine` and
the SLF4J sink belong to the application.

```scala
import cats.Eval
import cats.effect.IO
import cats.syntax.all.* // contramap and |+| come from cats syntax
import contratracer.ContraTracer
import org.slf4j.LoggerFactory

enum Level:
    case Debug, Info, Warn

final case class LogLine(logger: String, level: Level, message: Eval[String])

val slf4j: ContraTracer[IO, LogLine] = ContraTracer { line =>
    IO.delay {
        val log = LoggerFactory.getLogger(line.logger)
        line.level match
            case Level.Debug => if log.isDebugEnabled then log.debug(line.message.value)
            case Level.Info  => if log.isInfoEnabled then log.info(line.message.value)
            case Level.Warn  => if log.isWarnEnabled then log.warn(line.message.value)
    }
}

def format(e: FetcherEvent): LogLine = e match
    case FetcherEvent.Started(url) =>
        LogLine("fetcher", Level.Debug, Eval.later(s"fetching $url"))
    case FetcherEvent.Fetched(url, bytes) =>
        LogLine("fetcher", Level.Info, Eval.later(s"fetched $url, ${bytes.length} bytes"))
    case FetcherEvent.Failed(url, error) =>
        LogLine("fetcher", Level.Warn, Eval.later(s"fetch of $url failed: ${error.getMessage}"))

val fetcherTracer: ContraTracer[IO, FetcherEvent] = slf4j.contramap(format)
```

The component takes its tracer as a constructor parameter:

```scala
final class Fetcher(tracer: ContraTracer[IO, FetcherEvent]):
    def fetch(url: String): IO[Array[Byte]] =
        tracer.traceWith(FetcherEvent.Started(url)) >>
            download(url).flatTap(bytes => tracer.traceWith(FetcherEvent.Fetched(url, bytes)))
```

Code that should run under more than one effect takes `[F[_]: Monad]` and a
`ContraTracer[F, Event]`; the same code then traces in `IO`, `cats.Id` or `State`.

### Combine with `|+|`

`a |+| b` traces each event into `a`, then into `b`. `ContraTracer.nullTracer` traces nothing and is
the identity, so it is also `Monoid.empty`. Fold a list of tracers with `combineAll`:

```scala
val both: ContraTracer[IO, FetcherEvent] = fetcherTracer |+| metricsTracer
val all: ContraTracer[IO, FetcherEvent] = List(fetcherTracer, metricsTracer, auditTracer).combineAll
```

### Test with a capture tracer

The library has no capture tracer of its own; a `Ref` makes one in a line. Combine it with the real
sink to keep the log while capturing:

```scala
import cats.effect.{IO, Ref}

for
    seen <- Ref[IO].of(Vector.empty[FetcherEvent])
    capture = ContraTracer[IO, FetcherEvent](e => seen.update(_ :+ e))
    _ <- Fetcher(capture |+| fetcherTracer).fetch("https://example.org")
    events <- seen.get
yield assert(events.head == FetcherEvent.Started("https://example.org"))
```

Use a `Ref` or a concurrent queue, never a plain mutable buffer: a tracer may be called from several
fibers at once. Assert on each fiber's own order, or on membership; across fibers the order is not
fixed.

[`ContraTracerDemo.scala`](src/test/scala/contratracer/ContraTracerDemo.scala) is a worked
tour of `contramap`, `|+|`, `traceMaybe`, `squelchUnless`, `natTracer` and silencing.

## How a tracer behaves

- **Emitting or squelching.** Every tracer is one or the other, fixed when it is built.
  `nullTracer`, and anything composed only over it, squelches: `traceWith` returns `unit` without
  building the event or running any stage. One emitting part anywhere, including one leg of `|+|`,
  makes the whole tracer emitting.
- **The event is by-name.** An emitting tracer builds it exactly once, when `traceWith` is called.
- **What runs at the call.** In a monad that suspends, such as `IO`, the call evaluates the event
  and runs the function given to an `emit` leaf with nothing composed in front of it. Every other
  stage runs when the returned `IO` runs, and again each time it runs: in
  `slf4j.contramap(format)`, both `format` and the sink. In `Id`, everything runs at the call.
- **Order.** `a |+| b` traces into `a`, then `b`. A sink sees events in the order their effects run.
  Across fibers, only each fiber's own order holds.
- **Errors.** Nothing is caught. A failing leg of `|+|` stops the legs after it.
- **Sharing.** A tracer is an immutable value; share one across any number of fibers.
- **No queue, no thread.** A sink runs on the fiber that runs the traced effect.

The Scaladoc on `ContraTracer` gives each combinator's exact contract.

## Pitfalls

- **Build events cheaply; render in the format.** An emitting tracer builds every event and runs its
  format on every trace, whatever level the backend has enabled. Put values already in hand into
  the event: ids, sizes, references. Leave `toString`, hex encoding and pretty-printing to the
  format, and hand the message to the backend lazily, as `Eval.later` does above. A `val` holding
  the message in a format branch renders it before the level check.
- **Silence a tracer with `nullTracer`.** `ContraTracer(_ => IO.unit)` discards events, but it is
  emitting, so every event is still built and every stage in front of it still runs.
  `squelchUnless` does not silence either: over an emitting tracer, every event is still built and
  tested.
- **Trace before acting when the act can cancel you.** If an action's consequence can cancel the
  tracing fiber, a trace after it may never run. Trace first, or trace the outcome in a finaliser
  (`guaranteeCase`).
- **A blocking sink holds compute threads.** A sink that writes synchronously inside `IO.delay`
  blocks a compute worker and delays unrelated fibers. Log through an asynchronous appender, or wrap
  the write in `IO.blocking`, which can cost a thread handoff per trace. Keep an `emit` function
  itself cheap too, since it can run at the call.
- **Fold tracers with `combineAll`.** It nests to the right, which is stack-safe in `IO` and `Eval`.
  A hand-written left fold such as `tracers.reduce(_ |+| _)` recurses once per tracer when traced,
  and overflows the stack at a few thousand tracers.

## Layout

- `ContraTracer.scala`: the user-facing tracer (`traceWith`, `traceMaybe`, `squelchUnless`,
  `contramapM`, `traceTraversable`, `natTracer`, their `M` forms), its `Contravariant`,
  `Semigroup` and `Monoid` instances, and `ContraTracerSyntax` for a tracer given in scope.
- `TracerA.scala`: the underlying tagged Kleisli arrow (`Emitting` / `Squelching`), its `Category`,
  `Arrow` and `ArrowChoice` instances, and the compile step that makes a squelched trace
  allocation-free.

## Building

```bash
sbt compile
sbt test
sbt scalafmtCheckAll   # formatting
```

Uses sbt (see `project/build.properties`). A Nix flake provides a dev shell with a matching JDK, sbt
and scalafmt:

```bash
nix develop      # then run the sbt commands above
```

## License & attribution

Licensed under the [Apache License 2.0](LICENSE).

This is a derivative work: a Scala 3 / Cats port of the Haskell
[`contra-tracer`](https://github.com/avieth/contra-tracer) by Alexander Vieth, itself Apache-2.0
(Copyright 2019-2021 Input Output (Hong Kong) Ltd., Well-Typed LLP, and Alexander Vieth). See
[NOTICE](NOTICE) for the full attribution.
