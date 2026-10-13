package contratracer.keyed

import cats.Id
import cats.arrow.FunctionK
import contratracer.keyed.Model.*
import contratracer.{ContraTracer, KeyedTracer, TraceKey}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** Events as a sealed trait, each case with its key in its own companion. */
sealed trait WeaverEvent
object WeaverEvent {
    final case class Produced(block: Int) extends WeaverEvent
    object Produced { given TraceKey[Level, Produced] = TraceKey.const(Level.Info) }

    final case class Extracted(requests: List[Long]) extends WeaverEvent
    object Extracted { given TraceKey[Level, Extracted] = TraceKey.const(Level.Trace) }

    /** A wakeup from the future is a warning; one from the past is routine. */
    final case class WakeupIgnored(block: Int, isFuture: Boolean) extends WeaverEvent
    object WakeupIgnored {
        given TraceKey[Level, WakeupIgnored] =
            TraceKey.byValue[Level, WakeupIgnored](Level.Warn)(w =>
                if w.isFuture then Level.Warn else Level.Info
            )
    }

    case object Stopped extends WeaverEvent {
        given TraceKey[Level, Stopped.type] = TraceKey.const(Level.Info)
    }
}

/** Events as an enum, core's style: one key for the whole enum, gated at its most severe level. */
enum ComposerEvent:
    case Received(block: Int)
    case Confirmed(block: Int)
    case Stalled(block: Int)

object ComposerEvent {
    given TraceKey[Level, ComposerEvent] =
        TraceKey.byValue[Level, ComposerEvent](Level.Warn) {
            case Received(_)  => Level.Debug
            case Confirmed(_) => Level.Info
            case Stalled(_)   => Level.Warn
        }
}

class KeyedTracerSpec extends AnyFunSuite {
    import WeaverEvent.*

    test("traceAt: a disabled key builds nothing; control: an enabled key builds once") {
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](Logger.at(Level.Info), out)
        t.traceAt(Level.Debug)(boom[WeaverEvent])
        val _ = assertThrows[AssertionError](t.traceAt(Level.Info)(boom[WeaverEvent]))
        val builds = Builds()
        t.traceAt(Level.Info)(builds(Produced(1)))
        assert((builds.count, out.toList) == (1, List("Info Produced(1)")))
    }

    test("traceWith: a case class and a case object find their own key in their companion") {
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](Logger.at(Level.Info), out)
        val builds = Builds()
        t.traceWith(builds(Extracted(List(1L, 2L))))
        t.traceWith(builds(Produced(2)))
        t.traceWith(builds(Stopped))
        assert((builds.count, out.toList) == (2, List("Info Produced(2)", "Info Stopped")))
    }

    test("byValue gates on its bound, then traces at the event's own key") {
        val logger = Logger.at(Level.Warn)
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](logger, out)
        val builds = Builds()
        t.traceWith(builds(WakeupIgnored(1, isFuture = true)))
        t.traceWith(builds(WakeupIgnored(2, isFuture = false)))
        // Both are built, since Warn is enabled; only the Warn one reaches the sink.
        val _ = assert((builds.count, out.toList) == (2, List("Warn WakeupIgnored(1,true)")))
        logger.threshold = Some(Level.Error)
        t.traceWith(boom[WakeupIgnored])
    }

    test("a bound below the most severe key drops an enabled event") {
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](Logger.at(Level.Warn), out)
        val wrong: TraceKey[Level, WakeupIgnored] =
            TraceKey.byValue[Level, WakeupIgnored](Level.Info)(w =>
                if w.isFuture then Level.Warn else Level.Info
            )
        t.traceWith(WakeupIgnored(1, isFuture = true))(using wrong)
        val _ = assert(out.isEmpty)
        // control: the same event under the correct bound is traced
        t.traceWith(WakeupIgnored(1, isFuture = true))
        assert(out.toList == List("Warn WakeupIgnored(1,true)"))
    }

    test("enabled is asked on every trace, so a level changed at run time is followed") {
        val logger = Logger.off
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](logger, out)
        t.traceWith(boom[Produced])
        logger.threshold = Some(Level.Trace)
        t.traceWith(Extracted(Nil))
        assert(out.toList == List("Trace Extracted(List())"))
    }

    test("|+| with an always tracer: built for the recorder, refused by the sink") {
        val logger = Logger.at(Level.Warn)
        val out = ListBuffer.empty[String]
        val recorded = ListBuffer.empty[WeaverEvent]
        val recorder = KeyedTracer.always[Id, Level, WeaverEvent](
          ContraTracer.emit[Id, WeaverEvent](e => recorded += e: Unit)
        )
        val t = sink[WeaverEvent](logger, out) |+| recorder
        t.traceWith(Produced(1))
        val _ = assert((out.toList, recorded.toList) == (Nil, List(Produced(1))))
        // control: without the recorder, the same trace builds nothing
        sink[WeaverEvent](logger, out).traceWith(boom[Produced])
    }

    test("|+|: each leg receives only the keys its own enabled accepts") {
        val warn = ListBuffer.empty[String]
        val debug = ListBuffer.empty[String]
        val t =
            sink[WeaverEvent](Logger.at(Level.Warn), warn) |+|
                sink[WeaverEvent](Logger.at(Level.Debug), debug)
        t.traceWith(Produced(1))
        t.traceWith(WakeupIgnored(2, isFuture = true))
        t.traceWith(boom[Extracted])
        assert(
          (warn.toList, debug.toList) ==
              (
                List("Warn WakeupIgnored(2,true)"),
                List("Info Produced(1)", "Warn WakeupIgnored(2,true)")
              )
        )
    }

    test("contramap and mapK keep enabled") {
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](Logger.at(Level.Info), out)
        t.contramap[Int](Produced(_)).traceAt(Level.Debug)(boom[Int])
        t.mapK(FunctionK.id[Id]).traceAt(Level.Debug)(boom[WeaverEvent])
        t.contramap[Int](Produced(_)).traceAt(Level.Info)(3)
        t.mapK(FunctionK.id[Id]).traceAt(Level.Info)(Produced(4))
        assert(out.toList == List("Info Produced(3)", "Info Produced(4)"))
    }

    test("nullTracer builds nothing at any key, alone or as either leg of |+|") {
        val n = KeyedTracer.nullTracer[Id, Level, WeaverEvent]
        n.traceAt(Level.Error)(boom[WeaverEvent])
        n.traceWith(boom[Produced])
        val out = ListBuffer.empty[String]
        val t = sink[WeaverEvent](Logger.at(Level.Info), out)
        (n |+| t).traceWith(Produced(1))
        (t |+| n).traceWith(Produced(2))
        assert(out.toList == List("Info Produced(1)", "Info Produced(2)"))
    }

    test("enum, byValue: one instance serves every case, which the enum's apply widens to") {
        import ComposerEvent.*
        val logger = Logger.at(Level.Info)
        val out = ListBuffer.empty[String]
        val t = sink[ComposerEvent](logger, out)
        val builds = Builds()
        t.traceWith(builds(Received(1)))
        t.traceWith(builds(Confirmed(1)))
        t.traceWith(builds(Stalled(2)))
        val _ = assert(
          (builds.count, out.toList) == (3, List("Info Confirmed(1)", "Warn Stalled(2)"))
        )
        logger.threshold = Some(Level.Error)
        t.traceWith(boom[ComposerEvent])
    }

    test("enum, byValue: below the bound, a disabled case is still built, then refused") {
        import ComposerEvent.*
        val out = ListBuffer.empty[String]
        val t = sink[ComposerEvent](Logger.at(Level.Warn), out)
        // The gate asks Warn, the enum's most severe level, so this Info event is built.
        val _ = assertThrows[AssertionError](t.traceWith(boom[ComposerEvent]))
        val builds = Builds()
        t.traceWith(builds(Confirmed(1)))
        assert((builds.count, out.toList) == (1, Nil))
    }

    test("enum, traceAt: the level stated at the call builds nothing when disabled") {
        import ComposerEvent.*
        val out = ListBuffer.empty[String]
        val t = sink[ComposerEvent](Logger.at(Level.Warn), out)
        t.traceAt(Level.Info)(boom[ComposerEvent])
        t.traceAt(Level.Warn)(Stalled(3))
        assert(out.toList == List("Warn Stalled(3)"))
    }
}
