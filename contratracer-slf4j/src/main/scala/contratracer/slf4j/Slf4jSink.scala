package contratracer.slf4j

import cats.Monad
import cats.effect.kernel.Sync
import contratracer.{ContraTracer, KeyedTracer}
import org.slf4j.Logger

/** SLF4J sinks bound to one logger when they are wired, so a trace does no logger lookup. They call
  * SLF4J directly and leave the MDC alone.
  */
object Slf4jSink {

    /** A keyed sink that asks `logger` whether an event's level is enabled before the event is
      * built, on every trace, so a level changed at run time is followed.
      *
      * The level comes with the event: stated at the call with `traceAt`, or found as a
      * [[Severity]] by `traceWith`. `render` runs only for an event whose level was enabled at the
      * call, inside the `F[Unit]` the trace returns, and its message is logged at the event's
      * level. A level lowered between the call and the run still renders, and the logger drops the
      * message.
      */
    def keyed[F[_], A](logger: Logger)(render: A => String)(using
        F: Sync[F]
    ): KeyedTracer[F, Level, A] =
        KeyedTracer(
          Level.enabled(logger),
          ContraTracer.emit[F, (Level, A)]((level, a) => F.delay(log(logger, level, render(a))))
        )

    /** `tracer` if `logger` has any of `emits` enabled now, and `nullTracer` otherwise.
      *
      * `emits` is every level the component behind `tracer` logs at. The choice is made once, when
      * this is called, and `tracer` is not built when it is not chosen; a level raised later is not
      * followed.
      */
    def squelchedIfDisabled[F[_]: Monad, A](logger: Logger, emits: Iterable[Level])(
        tracer: => ContraTracer[F, A]
    ): ContraTracer[F, A] =
        if Level.anyEnabled(logger, emits) then tracer else ContraTracer.nullTracer[F, A]

    private def log(logger: Logger, level: Level, msg: String): Unit = level match {
        case Level.Trace => logger.trace(msg)
        case Level.Debug => logger.debug(msg)
        case Level.Info  => logger.info(msg)
        case Level.Warn  => logger.warn(msg)
        case Level.Error => logger.error(msg)
    }
}
