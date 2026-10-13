package contratracer

/** Five levels, least severe first, as the key of a [[KeyedTracer]]: the levels SLF4J, Logback and
  * Log4j share. The library gives them no meaning beyond their order; a sink decides what each one
  * enables.
  */
enum Level derives CanEqual:
    case Trace, Debug, Info, Warn, Error

object Level {

    /** A level's name for `traceAt`: `tracer.info(event)` is `tracer.traceAt(Level.Info)(event)`.
      * Each takes its event by name, so a disabled level builds nothing. They are found without an
      * import on any `KeyedTracer` keyed by `Level`.
      *
      * TRACE is `atTrace`, since `trace` would read as tracing at no level in particular.
      */
    extension [F[_], A](tracer: KeyedTracer[F, Level, A]) {
        def error(event: => A): F[Unit] = tracer.traceAt(Error)(event)
        def warn(event: => A): F[Unit] = tracer.traceAt(Warn)(event)
        def info(event: => A): F[Unit] = tracer.traceAt(Info)(event)
        def debug(event: => A): F[Unit] = tracer.traceAt(Debug)(event)
        def atTrace(event: => A): F[Unit] = tracer.traceAt(Trace)(event)
    }
}

/** The level every event of type `E` is traced at. Put the instance in `E`'s companion:
  * `given Severity[Produced] = Severity(Level.Info)`.
  */
type Severity[E] = TraceKey[Level, E]

object Severity {

    /** Every event of type `E` is traced at `level`. */
    def apply[E](level: Level): Severity[E] = TraceKey.const(level)

    /** The level depends on the event: `level(e)` names it, and `mostSevere` gates before the event
      * exists. `mostSevere` must be the most severe level `level` can return; with a less severe
      * one, an event is dropped whenever the sink disables `mostSevere`, even when the event's own
      * level is enabled.
      */
    def byValue[E](mostSevere: Level)(level: E => Level): Severity[E] =
        TraceKey.byValue(mostSevere)(level)
}
