package hydrozoa.lib.logging.edge

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

/** What a failure inside a tracer does to its caller. The library catches nothing: a failure
  * reaches the caller either thrown out of `traceWith` or raised in the returned `IO`, and which of
  * the two depends on whether the failing stage runs at call time or at run time (see
  * [[EvaluationTimingSpec]]). A failing leg of `|+|` stops the legs after it.
  */
class ThrowingSinkSpec extends AnyFunSuite {

    private final class Boom(where: String) extends RuntimeException(where)

    /** How a failure reached the caller of `traceWith`. */
    private enum Surfaced {
        case ThrownAtCall(where: String)
        case RaisedAtRun(where: String)
        case Completed
    }
    import Surfaced.*

    private def surfaced(traced: => IO[Unit]): Surfaced = Try(traced) match {
        case Failure(e: Boom) => ThrownAtCall(e.getMessage)
        case Failure(other)   => throw other
        case Success(io) =>
            io.attempt.unsafeRunSync() match {
                case Left(e: Boom) => RaisedAtRun(e.getMessage)
                case Left(other)   => throw other
                case Right(())     => Completed
            }
    }

    private val ok = ContraTracer.emit[IO, Int](_ => IO.unit)
    private def throwingFn(where: String) = ContraTracer.emit[IO, Int](_ => throw Boom(where))

    test("IO: a sink that raises fails the returned IO") {
        val t = ContraTracer.emit[IO, Int](_ => IO.raiseError(Boom("sink")))
        assert(surfaced(t.traceWith(1)) == RaisedAtRun("sink"))
    }

    test("IO: a sink that throws inside IO.delay fails the returned IO") {
        val t = ContraTracer.emit[IO, Int](_ => IO.delay(throw Boom("sink")))
        assert(surfaced(t.traceWith(1)) == RaisedAtRun("sink"))
    }

    test("IO: a sink function that throws is thrown out of traceWith when it is a bare leaf") {
        assert(surfaced(throwingFn("sink").traceWith(1)) == ThrownAtCall("sink"))
    }

    test("IO: the same throwing sink function is raised instead under contramap or a filter") {
        val outcomes = List(
          surfaced(throwingFn("sink").contramap[Int](identity).traceWith(1)),
          surfaced(throwingFn("sink").squelchUnless(_ => true).traceWith(1))
        )
        assert(outcomes == List.fill(2)(RaisedAtRun("sink")))
    }

    test("IO: a throwing contramap function, predicate or classifier is raised") {
        val outcomes = List(
          surfaced(ok.contramap[Int](_ => throw Boom("contramap")).traceWith(1)),
          surfaced(ok.squelchUnless(_ => throw Boom("predicate")).traceWith(1)),
          surfaced(ok.traceMaybe[Int](_ => throw Boom("classifier")).traceWith(1))
        )
        assert(outcomes == List("contramap", "predicate", "classifier").map(RaisedAtRun(_)))
    }

    test("IO: a throwing event is thrown out of traceWith, or raised when traced inside flatMap") {
        val outcomes = List(
          surfaced(ok.traceWith(throw Boom("event"))),
          surfaced(IO.unit.flatMap(_ => ok.traceWith(throw Boom("event"))))
        )
        assert(outcomes == List(ThrownAtCall("event"), RaisedAtRun("event")))
    }

    test("IO |+|: a left leg that fails stops the right leg, whichever way it fails") {
        val ran = ListBuffer.empty[String]
        val right = ContraTracer.emit[IO, Int](_ => IO.delay(ran += "right": Unit))
        val raisingLeft = ContraTracer.emit[IO, Int](_ => IO.raiseError(Boom("left")))
        val outcomes = List(
          surfaced((throwingFn("left") |+| right).traceWith(1)),
          surfaced((raisingLeft |+| right).traceWith(1))
        )
        assert((outcomes, ran.toList) == (List(ThrownAtCall("left"), RaisedAtRun("left")), Nil))
    }

    test("IO |+|: a right leg that fails has already let the left leg run") {
        val ran = ListBuffer.empty[String]
        val left = ContraTracer.emit[IO, Int](_ => IO.delay(ran += "left": Unit))
        val raisingRight = ContraTracer.emit[IO, Int](_ => IO.raiseError(Boom("right")))
        val outcomes = List(
          surfaced((left |+| throwingFn("right")).traceWith(1)),
          surfaced((left |+| raisingRight).traceWith(1))
        )
        assert((outcomes, ran.toList) == (List.fill(2)(RaisedAtRun("right")), List("left", "left")))
    }

    test("Id: every failure is thrown out of traceWith") {
        val okId = ContraTracer.emit[Id, Int](_ => ())
        val failing: List[(String, () => Id[Unit])] = List(
          "sink" -> (() => ContraTracer.emit[Id, Int](_ => throw Boom("sink")).traceWith(1)),
          "contramap" -> (() => okId.contramap[Int](_ => throw Boom("contramap")).traceWith(1)),
          "predicate" -> (() => okId.squelchUnless(_ => throw Boom("predicate")).traceWith(1)),
          "event" -> (() => okId.traceWith(throw Boom("event")))
        )
        failing.foreach { (where, trace) =>
            withClue(where)(assert(Try(trace()).failed.toOption.map(_.getMessage) == Some(where)))
        }
    }

    test("Id |+|: the legs run in order, and a failing leg stops the legs after it") {
        val ran = ListBuffer.empty[String]
        def mark(name: String) = ContraTracer.emit[Id, Int](_ => ran += name: Unit)
        val boom = ContraTracer.emit[Id, Int](_ => throw Boom("boom"))
        val failed = List(
          Try((boom |+| mark("right")).traceWith(1)).isFailure,
          Try((mark("left") |+| boom).traceWith(1)).isFailure
        )
        assert((failed, ran.toList) == (List(true, true), List("left")))
    }
}
