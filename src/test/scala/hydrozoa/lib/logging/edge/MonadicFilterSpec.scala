package hydrozoa.lib.logging.edge

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.logging.ContraTracer.nullTracer
import org.scalatest.funsuite.AnyFunSuite

/** `squelchUnlessM` and `traceMaybeM` run their effect only when something downstream emits, once
  * per run of the traced `IO`, and once for a whole `|+|` they gate. [[EvaluationTimingSpec]] shows
  * when; this counts how often.
  */
class MonadicFilterSpec extends AnyFunSuite {

    private final class Counter {
        var runs = 0
        def predicate(pass: Boolean): Int => IO[Boolean] = _ => IO.delay { runs += 1; pass }
    }

    private def leaf = ContraTracer.emit[IO, Int](_ => IO.unit)

    private def runsFor(gated: Counter => ContraTracer[IO, Int], runs: Int = 1): Int = {
        val counter = Counter()
        val traced = gated(counter).traceWith(1)
        (1 to runs).foreach(_ => traced.unsafeRunSync())
        counter.runs
    }

    test("the effect never runs when everything downstream squelches") {
        assert(runsFor(c => nullTracer[IO, Int].squelchUnlessM(c.predicate(true)), runs = 3) == 0)
    }

    test("the effect runs once per run of the traced IO, whether or not it passes") {
        val counts =
            List(true, false).map(pass => runsFor(c => leaf.squelchUnlessM(c.predicate(pass)), 3))
        assert(counts == List(3, 3))
    }

    test("gating a |+| runs the effect once; gating each leg runs it once per emitting leg") {
        val counts = List(
          runsFor(c => (leaf |+| leaf |+| leaf).squelchUnlessM(c.predicate(true))),
          runsFor(c =>
              leaf.squelchUnlessM(c.predicate(true)) |+| leaf.squelchUnlessM(c.predicate(true))
          ),
          runsFor(c =>
              leaf.squelchUnlessM(c.predicate(true)) |+| nullTracer[IO, Int].squelchUnlessM(
                c.predicate(true)
              )
          )
        )
        assert(counts == List(1, 2, 1))
    }

    test("traceMaybeM behaves the same: once per run, never when squelched") {
        val classify: Counter => Int => IO[Option[Int]] =
            c => i => c.predicate(true)(i).map(Option.when(_)(i))
        val counts = List(
          runsFor(c => leaf.traceMaybeM(classify(c)), runs = 2),
          runsFor(c => nullTracer[IO, Int].traceMaybeM(classify(c)), runs = 2)
        )
        assert(counts == List(2, 0))
    }
}
