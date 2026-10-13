package hydrozoa.lib.logging.edge

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean
import org.scalatest.funsuite.AnyFunSuite

/** A slow sink, seen from the one fiber or thread that traces. The library adds no asynchrony:
  * tracing is a step of the caller, so a sink that takes long makes the caller take long. A sink's
  * effect holds the fiber that runs the traced `IO`; a sink's function, when it runs at call time
  * (a bare `emit` leaf, the left leg of `|+|`), holds the thread that calls `traceWith`, before any
  * `IO` runs.
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

    test("IO: a bare leaf whose function blocks holds the caller of traceWith") {
        val entered = CountDownLatch(1)
        val release = CountDownLatch(1)
        val returned = AtomicBoolean(false)
        val blocking = ContraTracer.emit[IO, Int] { _ =>
            entered.countDown(); release.await(); IO.unit
        }
        val caller = Thread(() => { blocking.traceWith(1): Unit; returned.set(true) })
        caller.start()
        entered.await()
        val beforeRelease = returned.get
        release.countDown()
        caller.join()
        assert((beforeRelease, returned.get) == (false, true))
    }
}
