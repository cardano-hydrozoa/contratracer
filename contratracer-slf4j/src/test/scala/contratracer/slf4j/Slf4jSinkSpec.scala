package contratracer.slf4j

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level as LbLevel, Logger as LbLogger, LoggerContext}
import ch.qos.logback.core.read.ListAppender
import contratracer.{ContraTracer, KeyedTracer, Level, Severity}
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.funsuite.AnyFunSuite
import org.slf4j.LoggerFactory
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** The module's sinks against real Logback, whose levels the tests change while tracers run. */
class Slf4jSinkSpec extends AnyFunSuite {

    sealed trait Event
    object Event {
        final case class Produced(n: Int) extends Event
        object Produced { given Severity[Produced] = Severity(Level.Info) }
        final case class Extracted(n: Int) extends Event
        object Extracted { given Severity[Extracted] = Severity(Level.Trace) }
        final case class Ignored(n: Int, isFuture: Boolean) extends Event
        object Ignored {
            given Severity[Ignored] =
                Severity.byValue[Ignored](Level.Warn)(i =>
                    if i.isFuture then Level.Warn else Level.Info
                )
        }
    }
    import Event.*

    private def render(e: Event): String = e match {
        case Produced(n)   => s"produced $n"
        case Extracted(n)  => s"extracted $n"
        case Ignored(n, _) => s"ignored $n"
    }

    private def boom[A]: A = throw new AssertionError("the event was built")

    /** A fresh Logback logger at `level`, writing only to its own list appender. */
    private def logger(name: String, level: LbLevel): (LbLogger, ListAppender[ILoggingEvent]) = {
        val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
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

    test("an emitting sink that checks the level itself builds the event first") {
        val (lg, app) = logger("Keyed.unkeyed", LbLevel.OFF)
        val unkeyed = ContraTracer.emit[IO, (Level, Event)] { (l, e) =>
            IO(if Slf4jSink.enabled(lg)(l) then lg.info(render(e)))
        }
        val _ = assertThrows[AssertionError](unkeyed.traceWith((Level.Info, boom[Event])))
        assert(lines(app).isEmpty)
    }

    test("keyed: nothing is built below the logger's level; the event's level is logged") {
        val (lg, app) = logger("Keyed.levels", LbLevel.INFO)
        val t = Slf4jSink.keyed[IO, Event](lg)(render)
        t.traceAt(Level.Debug)(boom[Event]).unsafeRunSync()
        t.traceWith(boom[Extracted]).unsafeRunSync()
        (t.traceWith(Produced(1)) *> t.traceAt(Level.Warn)(Extracted(2))).unsafeRunSync()
        assert(lines(app) == List("INFO produced 1", "WARN extracted 2"))
    }

    test("keyed, control: at TRACE the same calls build their events") {
        val (lg, _) = logger("Keyed.control", LbLevel.TRACE)
        val t = Slf4jSink.keyed[IO, Event](lg)(render)
        val _ = assertThrows[AssertionError](t.traceAt(Level.Debug)(boom[Event]))
        val _ = assertThrows[AssertionError](t.traceWith(boom[Extracted]))
    }

    test("keyed: render runs only when the traced effect runs") {
        val (lg, app) = logger("Keyed.render", LbLevel.INFO)
        val renders = AtomicInteger()
        val t = Slf4jSink.keyed[IO, Event](lg) { e =>
            renders.incrementAndGet(); render(e)
        }
        val effect = t.traceWith(Produced(7))
        val before = renders.get
        effect.unsafeRunSync()
        assert((before, renders.get, lines(app)) == (0, 1, List("INFO produced 7")))
    }

    test("keyed: a level changed between traces is followed on the next trace") {
        val (lg, app) = logger("Keyed.runtime", LbLevel.WARN)
        val t = Slf4jSink.keyed[IO, Event](lg)(render)
        t.traceWith(boom[Produced]).unsafeRunSync()
        lg.setLevel(LbLevel.INFO)
        t.traceWith(Produced(3)).unsafeRunSync()
        lg.setLevel(LbLevel.ERROR)
        t.traceWith(boom[Produced]).unsafeRunSync()
        assert(lines(app) == List("INFO produced 3"))
    }

    test("keyed: a level changed while fibers trace stops and restarts the building") {
        val (lg, app) = logger("Keyed.concurrent", LbLevel.INFO)
        val t = Slf4jSink.keyed[IO, Event](lg)(render)
        val builds = AtomicInteger()
        val fibers = 4
        def tracing(running: Ref[IO, Boolean]): IO[Unit] =
            // traceWith builds its event at the call, so defer the call to make one per loop.
            (IO.defer(t.traceWith { builds.incrementAndGet(); Produced(0) }) >> IO.cede >>
                running.get)
                .iterateWhile(identity)
                .void
        val program = for {
            running <- Ref[IO].of(true)
            workers <- List.fill(fibers)(tracing(running).start).sequence
            _ <- IO.sleep(50.millis)
            _ <- IO(lg.setLevel(LbLevel.OFF))
            offAt <- IO(builds.get)
            _ <- IO.sleep(50.millis)
            offAfter <- IO(builds.get)
            loggedOff <- IO(app.list.size)
            _ <- IO(lg.setLevel(LbLevel.INFO))
            _ <- IO.sleep(50.millis)
            _ <- running.set(false)
            _ <- workers.traverse_(_.join)
        } yield (offAt, offAfter, loggedOff, app.list.size)
        val (offAt, offAfter, loggedOff, loggedEnd) = program.unsafeRunSync()
        info(s"built $offAt by OFF, $offAfter after; logged $loggedOff by INFO, $loggedEnd after")
        // A fiber that passed the level check before the flip may still build one event.
        val _ = assert(offAfter - offAt <= fibers, s"built $offAt then $offAfter while OFF")
        val _ = assert(offAt > 0, "nothing was built while INFO was enabled")
        assert(loggedEnd > loggedOff, "nothing was logged after INFO was re-enabled")
    }

    test("keyed: a field-dependent level gates on its most severe value, logs at its own") {
        val (lg, app) = logger("Keyed.byValue", LbLevel.WARN)
        val t = Slf4jSink.keyed[IO, Event](lg)(render)
        (t.traceWith(Ignored(1, isFuture = true)) *> t.traceWith(Ignored(2, isFuture = false)))
            .unsafeRunSync()
        val _ = assert(lines(app) == List("WARN ignored 1"))
        lg.setLevel(LbLevel.ERROR)
        t.traceWith(boom[Ignored]).unsafeRunSync()
    }

    test("keyed |+| always: the recorder gets every event, the logger only enabled ones") {
        val (lg, app) = logger("Keyed.recorder", LbLevel.WARN)
        val program = for {
            seen <- Ref[IO].of(Vector.empty[Event])
            recorder = KeyedTracer.always[IO, Level, Event](
              ContraTracer.emit[IO, Event](e => seen.update(_ :+ e))
            )
            t = Slf4jSink.keyed[IO, Event](lg)(render) |+| recorder
            _ <- t.traceWith(Produced(1)) >> t.traceWith(Ignored(2, isFuture = true))
            recorded <- seen.get
        } yield recorded
        val recorded = program.unsafeRunSync()
        assert(
          (recorded, lines(app)) == (Vector(Produced(1), Ignored(2, true)), List("WARN ignored 2"))
        )
    }

    test("squelchedIfDisabled: decided once at wiring; a later change is not followed") {
        val (lg, app) = logger("Keyed.wiring", LbLevel.OFF)
        val inner = ContraTracer.emit[IO, Event](e => IO(lg.info(render(e))))
        val t = Slf4jSink.squelchedIfDisabled(lg, List(Level.Info))(inner)
        t.traceWith(boom[Event]).unsafeRunSync()
        lg.setLevel(LbLevel.INFO)
        t.traceWith(Produced(5)).unsafeRunSync()
        val _ = assert(lines(app).isEmpty)
        // control: wired while enabled, the event is built and logged
        Slf4jSink
            .squelchedIfDisabled(lg, List(Level.Info))(inner)
            .traceWith(Produced(6))
            .unsafeRunSync()
        assert(lines(app) == List("INFO produced 6"))
    }
}
