package contratracer.shorthands

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level as LbLevel, Logger as LbLogger, LoggerContext}
import ch.qos.logback.core.read.ListAppender
import contratracer.KeyedTracer
import contratracer.slf4j.{Level, Slf4jSink}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*

/** `Level`'s shorthands, from a package that imports `Level` by name only: they resolve through the
  * tracer's type, and each leaves its event unbuilt when its level is disabled.
  */
class LevelShorthandsSpec extends AnyFunSuite {

    enum Event derives CanEqual:
        case Seen(n: Int)

    private def boom[A]: A = throw new AssertionError("the event was built")

    /** A logger context of this suite's own, so it never races another suite through SLF4J's global
      * initialisation, which hands out a substitute factory while it runs.
      */
    private val ctx = LoggerContext()

    private def logger(name: String, level: LbLevel): (LbLogger, ListAppender[ILoggingEvent]) = {
        val lg = ctx.getLogger(name)
        lg.setLevel(level)
        lg.setAdditive(false)
        val app = ListAppender[ILoggingEvent]()
        app.setContext(ctx)
        app.start()
        lg.addAppender(app)
        (lg, app)
    }

    private def lines(app: ListAppender[ILoggingEvent]): List[String] =
        app.list.asScala.toList.map(e => s"${e.getLevel} ${e.getFormattedMessage}")

    private type Shorthand = KeyedTracer[IO, Level, Event] => (=> Event) => IO[Unit]

    /** Each shorthand, with its level and the Logback level just above it. */
    private val shorthands: List[(String, Shorthand, LbLevel, LbLevel)] = List(
      ("error", t => e => t.error(e), LbLevel.ERROR, LbLevel.OFF),
      ("warn", t => e => t.warn(e), LbLevel.WARN, LbLevel.ERROR),
      ("info", t => e => t.info(e), LbLevel.INFO, LbLevel.WARN),
      ("debug", t => e => t.debug(e), LbLevel.DEBUG, LbLevel.INFO),
      ("atTrace", t => e => t.atTrace(e), LbLevel.TRACE, LbLevel.DEBUG)
    )

    for (name, shorthand, level, above) <- shorthands do {
        test(s"$name: builds nothing above its level; control: logs once at its level") {
            val (lg, app) = logger(s"Shorthands.$name", above)
            val t = Slf4jSink.keyed[IO, Event](lg)(_.toString)
            shorthand(t)(boom[Event]).unsafeRunSync()
            lg.setLevel(level)
            val _ = assertThrows[AssertionError](shorthand(t)(boom[Event]))
            shorthand(t)(Event.Seen(1)).unsafeRunSync()
            assert(lines(app) == List(s"$level Seen(1)"))
        }
    }

    test("at, hoisted from a Level-keyed sink, builds nothing when its level is disabled") {
        val (lg, app) = logger("Shorthands.at", LbLevel.WARN)
        val info = Slf4jSink.keyed[IO, Event](lg)(_.toString).at(Level.Info)
        info(boom[Event]).unsafeRunSync()
        lg.setLevel(LbLevel.INFO)
        info(Event.Seen(2)).unsafeRunSync()
        assert(lines(app) == List("INFO Seen(2)"))
    }
}
