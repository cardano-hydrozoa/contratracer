package hydrozoa.lib.logging.laws

import cats.Foldable
import cats.Eval
import cats.syntax.all.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Recorded.*
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.funsuite.AnyFunSuite

/** Inputs and stages that fail when forced, for laziness laws. */
object Boom:
    def apply[A](): A = throw new AssertionError("forced")

    /** A one-element container whose folds fail: tracing it forces the structure. */
    final case class Box[A](a: A)

    given Foldable[Box] with
        def foldLeft[A, B](fa: Box[A], b: B)(f: (B, A) => B): B = Boom()
        def foldRight[A, B](fa: Box[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = Boom()

/** Upstream's laziness properties (test/Control/Tracer/Properties.hs at 68fb6ed) and the library's
  * squelch law S1: a squelching tracer invokes no stage of its pipeline. Haskell's
  * `error "failure"` input becomes a stage that throws when invoked.
  */
object Laziness:
    import TracerA.given

    /** laziness_1: a pure stage on the input, then nothing emits. */
    val laziness1: CT[Int] = ContraTracer(
      TracerA.compute[W, Int, Boolean](_ => Boom()) >>> TracerA.compute[W, Boolean, Boolean](!_)
          >>> TracerA.squelch[W, Boolean]
    )

    /** laziness_2: choice between two null tracers. */
    val laziness2: CT[Int] = ContraTracer(
      TracerA.compute[W, Int, Either[Unit, Unit]](_ => Boom()) >>>
          (ContraTracer.nullTracer[W, Unit].use ||| ContraTracer.nullTracer[W, Unit].use)
    )

    /** laziness_3: the first branch emits, the second cannot; on `false` nothing after the branch
      * runs.
      */
    val laziness3: CT[Boolean] = ContraTracer(
      TracerA.compute[W, Boolean, Either[Unit, Unit]](b => if b then Left(()) else Right(())) >>>
          (TracerA.emit[W, Unit](_ => emitted(1, ())) |||
              (TracerA.effect[W, Unit, Either[Unit, Unit]](_ => Boom()) >>>
                  (ContraTracer.nullTracer[W, Unit].use ||| ContraTracer.nullTracer[W, Unit].use)))
    )

    /** traceTraversable_laziness: a null element tracer never folds the structure. */
    val traceTraversable: ContraTracer[W, Boom.Box[Int]] =
        ContraTracer.nullTracer[W, Int].traceTraversable[Boom.Box]

    /** traceAll_laziness: a null element tracer never computes the structure. */
    val traceAll: CT[Boolean] =
        ContraTracer.nullTracer[W, Int].traceAll[List, Boolean](_ => Boom())

    /** Every combinator over nullTracer, each stage throwing if invoked. */
    val squelchedPipeline: CT[Int] =
        val inner = ContraTracer.nullTracer[W, Int]
        val combined = inner.contramap[Int](_ => Boom()) |+|
            inner.contramapM[Int](_ => Boom()) |+|
            inner.squelchUnless(_ => Boom()) |+|
            inner.squelchUnlessM(_ => Boom()) |+|
            inner.traceMaybe[Int](_ => Boom()) |+|
            inner.traceMaybeM[Int](_ => Boom()) |+|
            inner.traceAll[List, Int](_ => Boom())
        combined.natTracer(toState).natTracer(Recorded.fromState)

class LazinessSuite extends AnyFunSuite:
    test("laziness_1: a pure stage before a squelch is never invoked") {
        assert(log(Laziness.laziness1.traceWith(0)).isEmpty)
    }

    test("laziness_2: choice between null tracers invokes nothing") {
        assert(log(Laziness.laziness2.traceWith(0)).isEmpty)
    }

    test("laziness_3: a branch that cannot emit is not run") {
        assert(log(Laziness.laziness3.traceWith(false)).isEmpty)
    }

    test("laziness_3: the emitting branch still emits") {
        assert(log(Laziness.laziness3.traceWith(true)) == List(Ev.Emit(1, "()")))
    }

    test("traceTraversable_laziness: a null element tracer never folds the structure") {
        assert(log(Laziness.traceTraversable.traceWith(Boom.Box(1))).isEmpty)
    }

    test("traceAll_laziness: a null element tracer never computes the structure") {
        assert(log(Laziness.traceAll.traceWith(true)).isEmpty)
    }

    test("S1 every combinator over nullTracer invokes none of its stages") {
        assert(log(Laziness.squelchedPipeline.traceWith(0)).isEmpty)
    }

    // Pending until traceWith takes its argument by name. Today the argument is evaluated before
    // traceWith is entered, so the counter reaches 1. When by-name lands, this block passes and
    // pendingUntilFixed fails the test: that failure is the signal to drop the wrapper.
    test("S1 a squelching tracer never forces its argument (needs by-name traceWith)") {
        pendingUntilFixed {
            val forced = new AtomicInteger(0)
            val _ = ContraTracer.nullTracer[W, Int].traceWith { forced.incrementAndGet(); 0 }
            assert(forced.get == 0): Unit
        }
    }
