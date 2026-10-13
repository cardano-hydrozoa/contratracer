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

The core library's only runtime dependency is `cats-core`. The optional modules under
[Modules](#modules) add their own.

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
format function. The core library has no backend; in this example, `LogLine` and the SLF4J sink
belong to the application, and `Level` is the library's five levels.

```scala
import cats.Eval
import cats.effect.IO
import contratracer.{ContraTracer, Level}
import org.slf4j.LoggerFactory

final case class LogLine(logger: String, level: Level, message: Eval[String])

val slf4j: ContraTracer[IO, LogLine] = ContraTracer { line =>
    IO.delay {
        val log = LoggerFactory.getLogger(line.logger)
        line.level match
            case Level.Trace => if log.isTraceEnabled then log.trace(line.message.value)
            case Level.Debug => if log.isDebugEnabled then log.debug(line.message.value)
            case Level.Info  => if log.isInfoEnabled then log.info(line.message.value)
            case Level.Warn  => if log.isWarnEnabled then log.warn(line.message.value)
            case Level.Error => if log.isErrorEnabled then log.error(line.message.value)
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
the identity, so it is also `Monoid.empty`; `a |+| nullTracer` is `a` itself, at no cost. Fold a list of tracers with `combineAll`. Like
`contramap`, `|+|` is a method on the tracer; only `combineAll` needs cats syntax:

```scala
import cats.syntax.all.* // combineAll

val both: ContraTracer[IO, FetcherEvent] = fetcherTracer |+| metricsTracer
val all: ContraTracer[IO, FetcherEvent] = List(fetcherTracer, metricsTracer, auditTracer).combineAll
```

### Test with a recording tracer

`Recording.make`, in the `contratracer-effect` module, returns a tracer that keeps every event in a
`Ref`, and the effect that reads them back. Combine it with the real sink to keep the log while
recording:

```scala
import cats.effect.IO
import contratracer.effect.Recording

for
    recording <- Recording.make[IO, FetcherEvent]
    (recorder, recorded) = recording
    _ <- Fetcher(recorder |+| fetcherTracer).fetch("https://example.org")
    events <- recorded
yield assert(events.head == FetcherEvent.Started("https://example.org"))
```

A tracer may be called from several fibers at once, so a hand-written recorder needs a `Ref` or a
concurrent queue, never a plain mutable buffer. Assert on each fiber's own order, or on membership;
across fibers the order is not fixed.

[`ContraTracerDemo.scala`](contratracer/src/test/scala/contratracer/ContraTracerDemo.scala) is a worked
tour of `contramap`, `|+|`, `traceMaybe`, `squelchUnless`, `mapK` and silencing.

### Skip disabled events with `KeyedTracer`

An emitting tracer builds every event, because only the sink knows whether the backend wants it. A
`KeyedTracer[M, K, A]` asks first: each event travels with a key known before the event exists,
such as a level, and `enabled(key)` decides whether the event is built at all.

State the key at the call with `traceAt`, or put a `TraceKey` beside the event type and call
`traceWith`, which finds it from the argument's static type:

```scala
import contratracer.{KeyedTracer, Level, TraceKey}

sealed trait WeaverEvent
object WeaverEvent:
    final case class Produced(block: Int) extends WeaverEvent
    object Produced:
        given TraceKey[Level, Produced] = TraceKey.const(Level.Info)

    final case class WakeupIgnored(block: Int, isFuture: Boolean) extends WeaverEvent
    object WakeupIgnored:
        given TraceKey[Level, WakeupIgnored] =
            TraceKey.byValue(Level.Warn)(w => if w.isFuture then Level.Warn else Level.Info)

def weaver(tracer: KeyedTracer[IO, Level, WeaverEvent]): IO[Unit] =
    tracer.traceWith(WeaverEvent.Produced(1)) >>
        tracer.traceAt(Level.Debug)(WeaverEvent.Produced(2))
```

- **`TraceKey.byValue(bound)(keyOf)`** is for a key that depends on the event's fields. `bound`
  gates before the event is built, then `keyOf` names the event's own key. `bound` must be the most
  severe key the event can take; a lower one drops events whose own key is enabled.
- **Enums.** An enum case's `apply` returns the enum type, so `traceWith(Received(1))` looks for an
  instance for the whole enum. Give the enum one `TraceKey.byValue` at its most severe level, or
  state the level at the call with `traceAt`. Under the first, a case below the enum's most severe
  level is built whenever that level is enabled, and then refused.
- **`at(key)`** fixes the key: `tracer.at(Level.Info)(event)` is `traceAt(Level.Info)(event)`. Hoist
  it once, as `val info = tracer.at(Level.Info)`, and call `info(event)`. The hoisted function still
  takes its event by name and asks `enabled` on every call.
- **`Level`** is five levels, `Trace` to `Error`, for a key that is a level, and **`Severity[E]`** is
  `TraceKey[Level, E]`, written `Severity(Level.Info)` or `Severity.byValue(Level.Warn)(levelOf)`.
- **Level shorthands.** On any `KeyedTracer` keyed by `Level`, `tracer.info(event)` is
  `tracer.traceAt(Level.Info)(event)`, and likewise `error`, `warn` and `debug`; TRACE is `atTrace`,
  since `trace` would read as tracing at no level in particular. They live in `Level`'s companion,
  so they need no import, and each takes its event by name.
- **Composition.** `contramap`, `|+|` and `mapK` work as on `ContraTracer`. With `|+|`, a key is
  enabled when either side enables it, and each side still receives only the keys it enables.
  `KeyedTracer.always(tracer)` enables every key, for a recording tracer in tests;
  `KeyedTracer.nullTracer` enables none and is the identity: `t |+| KeyedTracer.nullTracer` is
  `t` itself.

`contratracer-slf4j` provides a keyed sink over an SLF4J logger.

## How a tracer behaves

- **Emitting or squelching.** Every tracer is one or the other, fixed when it is built.
  `nullTracer`, and anything composed only over it, squelches: `traceWith` returns `unit` without
  building the event or running any stage. One emitting part anywhere, including one leg of `|+|`,
  makes the whole tracer emitting.
- **The event is by-name.** An emitting tracer builds it exactly once, when `traceWith` is called.
- **What runs at the call.** Only the event. Every stage and sink runs when the returned effect
  runs, and again each time it runs: in `slf4j.contramap(format)`, both `format` and the sink. In
  `Id`, that is at the call.
- **Order.** `a |+| b` traces into `a`, then `b`. A sink sees events in the order their effects run.
  Across fibers, only each fiber's own order holds.
- **Errors.** Nothing is caught. An event that fails to build is thrown out of `traceWith`; a
  failing stage or sink fails the returned effect. A failing leg of `|+|` stops the legs after it.
- **Sharing.** A tracer is an immutable value; share one across any number of fibers.
- **No queue, no thread.** A sink runs on the fiber that runs the traced effect.

The Scaladoc on `ContraTracer` gives each combinator's exact contract.

## Pitfalls

- **Build events cheaply; render in the format.** An emitting tracer builds every event and runs its
  format on every trace, whatever level the backend has enabled. Put values already in hand into
  the event: ids, sizes, references. Leave `toString`, hex encoding and pretty-printing to the
  format, and hand the message to the backend lazily, as `Eval.later` does above. A `val` holding
  the message in a format branch renders it before the level check. To skip building events the
  backend would drop, trace through a `KeyedTracer`.
- **Silence a tracer with `nullTracer`.** `ContraTracer(_ => IO.unit)` discards events, but it is
  emitting, so every event is still built and every stage in front of it still runs.
  `squelchUnless` does not silence either: over an emitting tracer, every event is still built and
  tested.
- **Trace before acting when the act can cancel you.** If an action's consequence can cancel the
  tracing fiber, a trace after it may never run. Trace first, or trace the outcome in a finaliser.
  `contratracer-effect` has both as combinators: `traceThenAct` and `actThenTraceOutcome`.
- **A blocking sink holds compute threads.** A sink that writes synchronously inside `IO.delay`
  blocks a compute worker and delays unrelated fibers. Log through an asynchronous appender, or wrap
  the write in `IO.blocking`, which can cost a thread handoff per trace.
- **Deep compositions overflow in `Id`.** In `IO` and `Eval`, tracing is stack-safe at any depth of
  `contramap`, filters or `|+|`, folded in either direction. `Id` has no stack-safe `flatMap`, so
  tracing recurses once per level and can overflow a default thread stack from about a thousand
  levels.

## Modules

Each module depends on the core library and is published beside it from the same build. JitPack
documents multi-module coordinates as the group `com.github.cardano-hydrozoa.contratracer` with the
module's name as the artifact.

### `contratracer-slf4j`

Keyed sinks over SLF4J, with no logging backend. It adds `slf4j-api` and `cats-effect-kernel`.

- **`Slf4jSink.keyed(logger)(render)`** is a `KeyedTracer[F, Level, A]` bound to one logger when
  it is wired. It asks the logger whether the event's level is enabled before the event is built,
  on every trace, so a level changed at run time is followed. `render` runs only for an event whose
  level was enabled at the call, when the traced effect runs. It calls SLF4J directly and leaves
  the MDC alone.
- **`Slf4jSink.enabled(logger)(level)`** asks the logger whether `level` is enabled, as `keyed`
  does; `Slf4jSink.anyEnabled(logger, levels)` asks it of several.
- **`Slf4jSink.squelchedIfDisabled(logger, levels)(tracer)`** returns `nullTracer` when the logger
  has none of `levels` enabled at wiring. It never follows a later level change.

With `WeaverEvent`'s keys written over the library's `Level`, as
`given Severity[Produced] = Severity(Level.Info)`, the events wire straight to a logger:

```scala
import contratracer.{Level, Severity}
import contratracer.slf4j.Slf4jSink
import org.slf4j.LoggerFactory

val weaverTracer: KeyedTracer[IO, Level, WeaverEvent] =
    Slf4jSink.keyed[IO, WeaverEvent](LoggerFactory.getLogger("weaver")) {
        case WeaverEvent.Produced(n)         => s"produced block $n"
        case WeaverEvent.WakeupIgnored(n, _) => s"ignored a wakeup for block $n"
    }
```

### `contratracer-effect`

Combinators that need cats-effect. It adds `cats-effect-kernel`.

- **`Recording.make[F, A]`** returns a tracer that keeps every event in a `Ref`, and the effect that
  reads them, oldest first. Each fiber's events keep the order their traces ran.
- **`Recording.awaiting(p)`** adds an effect that waits for the first event satisfying `p` and
  returns it. The event is recorded before the waiter wakes, so a test can wait for a trace instead
  of sleeping.
- **`TraceAroundAct.traceThenAct(tracer)(event)(act)`** traces, then acts, as one uncancelable step:
  the event is traced if and only if the act runs. The whole act is masked, so keep it short.
- **`TraceAroundAct.actThenTraceOutcome(tracer)(act)(outcome => event)`** acts, then traces how the
  act ended in a finaliser that a cancel cannot skip. The trace can trail what observers of the act
  already saw.

## Layout

Each published module has its own directory; the build's root aggregates them and publishes
nothing.

- `contratracer/`: the core library, in package `contratracer`.
- `contratracer-slf4j/`: the SLF4J module.
- `contratracer-effect/`: the cats-effect module.

The core's sources:

- `ContraTracer.scala`: the user-facing tracer (`traceWith`, `contramap`, `|+|`, `traceMaybe`,
  `squelchUnless`, `contramapM`, `traceTraversable`, `mapK`, their `M` forms) and its
  `Contravariant` and `Monoid` instances.
- `TracerA.scala`: the underlying tagged Kleisli arrow (`Emitting` / `Squelching`), its
  `ArrowChoice` instance, and the compile step that makes a squelched trace allocation-free.
- `KeyedTracer.scala`: `KeyedTracer`, which asks with a key before building an event, and
  `TraceKey`, the key an event's type carries.
- `Level.scala`: `Level`, its shorthands, and `Severity`, a `TraceKey` over `Level`.

## Building

```bash
sbt compile
sbt test
sbt scalafmtCheckAll   # formatting
```

The cats law suites check each law 1,000 times. Before a release, run them at 5,000 on three seeds,
as `LawEvaluations` describes.

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
