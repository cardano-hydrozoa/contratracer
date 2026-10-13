package contratracer.edge

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import contratracer.ContraTracer
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.funsuite.AnyFunSuite

/** A slow sink, seen from the one fiber or thread that traces. The library adds no asynchrony:
  * tracing is a step of the caller, so a sink that takes long makes the caller take long. In `IO` a
  * sink's function and its effect both run when the traced `IO` runs, so they hold the fiber that
  * runs it and never the thread that calls `traceWith`.
  */
class SlowSinkSpec extends AnyFunSuite {

    /** Starts `traced` then a marker on one fiber, waits until the slow sink is entered, and
      * reports whether the marker had run then and after the sink was released.
      */
    private def markerBeforeAndAfterRelease(
        tracer: (IO[Unit], IO[Unit]) => ContraTracer[IO, Int]
    ): (Boolean, Boolean) = {
        val program = for {
            entered <- Deferred[IO, Unit]
            release <- Deferred[IO, Unit]
            marker <- Ref[IO].of(false)
            traced = tracer(entered.complete(()).void, release.get)
            fiber <- (traced.traceWith(1) *> marker.set(true)).start
            _ <- entered.get
            before <- marker.get
            _ <- release.complete(())
            _ <- fiber.joinWithNever
            after <- marker.get
        } yield (before, after)
        program.unsafeRunSync()
    }

    test("IO: the step after a trace waits for a slow sink's effect") {
        val observed = markerBeforeAndAfterRelease { (enter, slow) =>
            ContraTracer.emit[IO, Int](_ => enter *> slow)
        }
        assert(observed == (false, true))
    }

    test("IO |+|: the right leg waits for a slow left leg") {
        val observed = markerBeforeAndAfterRelease { (enter, slow) =>
            ContraTracer.emit[IO, Int](_ => enter *> slow) |+|
                ContraTracer.emit[IO, Int](_ => IO.unit)
        }
        assert(observed == (false, true))
    }

    test("IO: a bare leaf's function runs when the IO runs, not at the traceWith call") {
        val calls = AtomicInteger(0)
        val leaf = ContraTracer.emit[IO, Int] { _ =>
            calls.incrementAndGet(); IO.unit
        }
        val traced = leaf.traceWith(1)
        val atCall = calls.get
        traced.unsafeRunSync()
        assert((atCall, calls.get) == (0, 1))
    }
}
