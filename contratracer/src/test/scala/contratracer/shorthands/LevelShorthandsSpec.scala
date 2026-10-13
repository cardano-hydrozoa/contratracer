package contratracer.shorthands

import cats.Id
import contratracer.{ContraTracer, KeyedTracer, Level}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** `Level`'s shorthands, from a package that imports `Level` by name only: they resolve through the
  * tracer's type, and each leaves its event unbuilt when its level is disabled.
  */
class LevelShorthandsSpec extends AnyFunSuite {

    enum Event derives CanEqual:
        case Seen(n: Int)

    private def boom[A]: A = throw new AssertionError("the event was built")

    /** A tracer enabling `threshold` and above, which writes `"<level> <event>"` lines to `out`. */
    private def sink(threshold: Level, out: ListBuffer[String]): KeyedTracer[Id, Level, Event] =
        KeyedTracer(
          _.ordinal >= threshold.ordinal,
          ContraTracer.emit[Id, (Level, Event)]((l, e) => out += s"$l $e": Unit)
        )

    private type Shorthand = KeyedTracer[Id, Level, Event] => (=> Event) => Unit

    /** Each shorthand, with its level and the level just above it, if any. */
    private val shorthands: List[(String, Shorthand, Level, Option[Level])] = List(
      ("error", t => e => t.error(e), Level.Error, None),
      ("warn", t => e => t.warn(e), Level.Warn, Some(Level.Error)),
      ("info", t => e => t.info(e), Level.Info, Some(Level.Warn)),
      ("debug", t => e => t.debug(e), Level.Debug, Some(Level.Info)),
      ("atTrace", t => e => t.atTrace(e), Level.Trace, Some(Level.Debug))
    )

    for (name, shorthand, level, above) <- shorthands do {
        test(s"$name: builds nothing above its level; control: traces once at its level") {
            val out = ListBuffer.empty[String]
            above.foreach(a => shorthand(sink(a, out))(boom[Event]))
            shorthand(KeyedTracer.nullTracer[Id, Level, Event])(boom[Event])
            val _ = assertThrows[AssertionError](shorthand(sink(level, out))(boom[Event]))
            shorthand(sink(level, out))(Event.Seen(1))
            assert(out.toList == List(s"$level Seen(1)"))
        }
    }

    test("at, hoisted over Level, builds nothing when its level is disabled") {
        val out = ListBuffer.empty[String]
        val info = sink(Level.Warn, out).at(Level.Info)
        info(boom[Event])
        sink(Level.Info, out).at(Level.Info)(Event.Seen(2))
        assert(out.toList == List("Info Seen(2)"))
    }
}
