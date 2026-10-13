package contratracer.keyed

import cats.Id
import contratracer.{ContraTracer, KeyedTracer}
import scala.collection.mutable.ListBuffer

/** A logging backend reduced to what decides whether an event is built: five levels and a logger
  * whose threshold a test may move at any time, as Logback's can.
  */
object Model {

    enum Level:
        case Trace, Debug, Info, Warn, Error

    final class Logger(var threshold: Option[Level]) {
        def isEnabled(level: Level): Boolean = threshold.exists(_.ordinal <= level.ordinal)
    }

    object Logger {
        def off: Logger = Logger(None)
        def at(level: Level): Logger = Logger(Some(level))
    }

    /** A keyed sink over `logger` that writes `"<level> <event>"` lines to `out`. */
    def sink[A](logger: Logger, out: ListBuffer[String]): KeyedTracer[Id, Level, A] =
        KeyedTracer(
          logger.isEnabled,
          ContraTracer.emit[Id, (Level, A)]((l, a) => out += s"$l $a": Unit)
        )

    /** An argument that fails the test if anything evaluates it. */
    def boom[A]: A = throw new AssertionError("the event was built")

    /** Counts how many times an argument expression was evaluated. */
    final class Builds {
        var count = 0
        def apply[A](a: A): A = { count += 1; a }
    }
}
