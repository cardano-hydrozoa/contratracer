package contratracer.effect

import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import contratracer.ContraTracer
import contratracer.effect.TraceAroundAct.*
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

/** A trace placed after an act can be lost when the act's consequence cancels the tracing fiber.
  * Each scenario runs twice: under `TestControl`, where time is simulated and the run is
  * deterministic, and on the real runtime, where it rests on timing margins.
  */
class TraceAroundActSpec extends AnyFunSuite {

    /** How long the slow tracer takes to write an event. */
    private val TraceTime = 250.millis

    /** A tracer slower than the cancel its act provokes. */
    private val slowCapture: IO[(ContraTracer[IO, String], IO[List[String]])] =
        Ref[IO].of(List.empty[String]).map { r =>
            (ContraTracer.emit[IO, String](e => IO.sleep(TraceTime) >> r.update(_ :+ e)), r.get)
        }

    private val runners: List[(String, IO[(Boolean, List[String])] => (Boolean, List[String]))] =
        List(
          "TestControl" -> (io => TestControl.executeEmbed(io).unsafeRunSync()),
          "real runtime" -> (io => io.unsafeRunSync())
        )

    /** Runs `body` in a fiber that is cancelled as soon as its act (completing `acted`) is seen,
      * standing for the peer that hangs up on reading a reply. Returns whether the act happened and
      * what was traced.
      */
    private def run(
        body: (ContraTracer[IO, String], Deferred[IO, Unit]) => IO[Unit]
    ): IO[(Boolean, List[String])] = for {
        capture <- slowCapture
        (t, read) = capture
        acted <- Deferred[IO, Unit]
        fiber <- body(t, acted).start
        _ <- acted.get.timeout(4 * TraceTime).attempt
        _ <- fiber.cancel
        didAct <- acted.tryGet.map(_.isDefined)
        traced <- read
    } yield (didAct, traced)

    /** Runs `body` in a fiber cancelled 50 ms into the slow tracer's 250 ms write. */
    private def cancelMidTrace(
        body: (ContraTracer[IO, String], Deferred[IO, Unit]) => IO[Unit]
    ): IO[(Boolean, List[String])] = for {
        capture <- slowCapture
        (t, read) = capture
        acted <- Deferred[IO, Unit]
        fiber <- body(t, acted).start
        _ <- IO.sleep(TraceTime / 5) >> fiber.cancel
        didAct <- acted.tryGet.map(_.isDefined)
        traced <- read
    } yield (didAct, traced)

    private def act(acted: Deferred[IO, Unit]): IO[Unit] = acted.complete(()).void

    runners.foreach { (runtime, runIO) =>
        test(s"$runtime: act, then trace: a cancel provoked by the act loses the trace") {
            val (didAct, traced) = runIO(run((t, acted) => act(acted) >> t.traceWith("acted")))
            assert(didAct && traced.isEmpty)
        }

        test(s"$runtime: trace, then act: no act goes untraced") {
            val (didAct, traced) = runIO(run((t, acted) => t.traceWith("acting") >> act(acted)))
            assert(didAct && traced == List("acting"))
        }

        test(s"$runtime: actThenTraceOutcome: the trace survives the cancel") {
            val (didAct, traced) = runIO(
              run((t, acted) =>
                  actThenTraceOutcome(t)(act(acted)) {
                      case Outcome.Succeeded(_) => "acted"
                      case Outcome.Errored(e)   => s"failed: $e"
                      case Outcome.Canceled()   => "cancelled"
                  }
              )
            )
            // A cancel can land inside the act after its visible effect, so either is possible.
            info(s"traced: $traced")
            assert(didAct && (traced == List("acted") || traced == List("cancelled")))
        }

        test(s"$runtime: actThenTraceOutcome traces a failed act") {
            val (didAct, traced) = runIO(
              run((t, _) =>
                  actThenTraceOutcome(t)(IO.raiseError[Unit](new RuntimeException("refused"))) {
                      case Outcome.Errored(e) => s"failed: ${e.getMessage}"
                      case other              => s"unexpected: $other"
                  }.attempt.void
              )
            )
            assert(!didAct && traced == List("failed: refused"))
        }

        test(s"$runtime: a cancel between trace and act: plain sequencing traces no act") {
            val (didAct, traced) =
                runIO(run((t, acted) => t.traceWith("acting") >> IO.canceled >> act(acted)))
            assert(!didAct && traced == List("acting"))
        }

        test(s"$runtime: traceThenAct: a cancel between trace and act waits for the act") {
            val (didAct, traced) =
                runIO(run((t, acted) => traceThenAct(t)("acting")(IO.canceled >> act(acted))))
            assert(didAct && traced == List("acting"))
        }

        test(s"$runtime: a cancel mid-trace: trace, then act loses both") {
            val (didAct, traced) =
                runIO(cancelMidTrace((t, acted) => t.traceWith("acting") >> act(acted)))
            assert(!didAct && traced.isEmpty)
        }

        test(s"$runtime: a cancel mid-trace: traceThenAct keeps both") {
            val (didAct, traced) =
                runIO(cancelMidTrace((t, acted) => traceThenAct(t)("acting")(act(acted))))
            assert(didAct && traced == List("acting"))
        }
    }

    test("traceThenAct builds its event each time the effect runs") {
        val program = for {
            capture <- Recording.make[IO, Int]
            (t, read) = capture
            acts <- Ref[IO].of(0)
            counter = new java.util.concurrent.atomic.AtomicInteger()
            step = traceThenAct(t)(counter.incrementAndGet())(acts.update(_ + 1))
            before = counter.get
            _ <- step >> step
            seen <- read
        } yield (before, seen)
        assert(TestControl.executeEmbed(program).unsafeRunSync() == (0, Vector(1, 2)))
    }
}
