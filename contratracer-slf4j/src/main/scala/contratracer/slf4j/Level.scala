package contratracer.slf4j

import contratracer.{KeyedTracer, TraceKey}
import org.slf4j.Logger

/** SLF4J's five levels, least severe first, as the key of a `KeyedTracer`. */
enum Level derives CanEqual:
    case Trace, Debug, Info, Warn, Error

object Level {

    /** Whether `logger` has `level` enabled, asked of the logger on every call, so a level changed
      * at run time is followed. It makes the same call SLF4J's default `isEnabledForLevel` makes.
      */
    def enabled(logger: Logger)(level: Level): Boolean = level match {
        case Trace => logger.isTraceEnabled
        case Debug => logger.isDebugEnabled
        case Info  => logger.isInfoEnabled
        case Warn  => logger.isWarnEnabled
        case Error => logger.isErrorEnabled
    }

    /** Whether `logger` has any of `levels` enabled. */
    def anyEnabled(logger: Logger, levels: Iterable[Level]): Boolean =
        levels.exists(enabled(logger))

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

/** The level every event of type `E` is logged at. Put the instance in `E`'s companion:
  * `given Severity[Produced] = Severity(Level.Info)`.
  */
type Severity[E] = TraceKey[Level, E]

object Severity {

    /** Every event of type `E` is logged at `level`. */
    def apply[E](level: Level): Severity[E] = TraceKey.const(level)

    /** The level depends on the event: `level(e)` names it, and `mostSevere` gates before the event
      * exists. `mostSevere` must be the most severe level `level` can return; with a less severe
      * one, an event is dropped whenever the logger is above it, even when the event's own level is
      * enabled.
      */
    def byValue[E](mostSevere: Level)(level: E => Level): Severity[E] =
        TraceKey.byValue(mostSevere)(level)
}
