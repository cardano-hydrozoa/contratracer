package contratracer.edge

import cats.arrow.FunctionK
import cats.syntax.all.*
import contratracer.ContraTracer
import contratracer.ContraTracer.nullTracer
import org.scalatest.funsuite.AnyFunSuite

/** When each part of a composed tracer is evaluated, relative to `traceWith` returning and to the
  * returned `M[Unit]` being run, in each monad.
  *
  * Two things decide it. `traceWith` applies the compiled tracer to the event as soon as it is
  * called, so the event and every stage the library builds as a bare `Kleisli` (an `emit` leaf, the
  * left leg of `|+|`) run at call time. Stages joined with cats' `Kleisli.andThen` (every
  * `contramap`, `contramapM`, `squelchUnless`, `traceMaybe` stage) go through `Kleisli.shift`,
  * which puts them behind a `flatMap(pure(a))`: in a suspending monad they run when the result is
  * run, and again on every further run. In `Id` there is no difference: everything runs at call
  * time, once.
  */
class EvaluationTimingSpec extends AnyFunSuite {

    /** What a shape logs at call time and on each run, in a suspending monad. */
    private case class Expected(atCall: List[String], perRun: List[String])

    private def shapes[M[_]](r: Runner[M], log: PhaseLog): List[(String, ContraTracer[M, Int])] = {
        import r.given
        def leaf(tag: String) = ContraTracer.emit[M, Int] { i =>
            log(s"$tag-fn($i)"); r.delay(log(s"$tag-effect($i)"))
        }
        List(
          "emit" -> leaf("sink"),
          "contramap" -> leaf("sink").contramap[Int] { i =>
              log("contramap"); i + 1
          },
          "contramap twice" -> leaf("sink")
              .contramap[Int] { i =>
                  log("inner"); i + 1
              }
              .contramap[Int] { i =>
                  log("outer"); i + 1
              },
          "contramapM" -> leaf("sink").contramapM[Int] { i =>
              log("contramapM-fn"); r.delay(log("contramapM-effect")).as(i)
          },
          "squelchUnless (passes)" -> leaf("sink").squelchUnless { _ =>
              log("p"); true
          },
          "squelchUnless (filtered)" -> leaf("sink").squelchUnless { _ =>
              log("p"); false
          },
          "traceMaybe (Some)" -> leaf("sink").traceMaybe[Int] { i =>
              log("classify"); Some(i)
          },
          "squelchUnlessM (passes)" -> leaf("sink").squelchUnlessM { _ =>
              log("pM-fn"); r.delay(log("pM-effect")).as(true)
          },
          "traceMaybeM (None)" -> leaf("sink").traceMaybeM[Int] { _ =>
              log("classifyM-fn"); r.delay(log("classifyM-effect")).as(None)
          },
          "L |+| R" -> (leaf("L") |+| leaf("R")),
          "R |+| L" -> (leaf("R") |+| leaf("L")),
          "L.contramap |+| R" -> (leaf("L").contramap[Int] { i =>
              log("cmL"); i
          } |+| leaf("R")),
          "(L |+| R) |+| S" -> ((leaf("L") |+| leaf("R")) |+| leaf("S")),
          "L |+| (R |+| S)" -> (leaf("L") |+| (leaf("R") |+| leaf("S"))),
          "emit |+| squelched contramap" ->
              (leaf("sink") |+| nullTracer[M, Int].contramap[Int] { i =>
                  log("sq"); i
              }),
          "squelched contramap |+| emit" ->
              (nullTracer[M, Int].contramap[Int] { i =>
                  log("sq"); i
              } |+| leaf("sink")),
          "natTracer(id)" -> leaf("sink").natTracer(FunctionK.id[M])
        )
    }

    private val expected: Map[String, Expected] = Map(
      "emit" -> Expected(List("event", "sink-fn(7)"), List("sink-effect(7)")),
      "contramap" -> Expected(List("event"), List("contramap", "sink-fn(8)", "sink-effect(8)")),
      "contramap twice" ->
          Expected(List("event"), List("outer", "inner", "sink-fn(9)", "sink-effect(9)")),
      "contramapM" -> Expected(
        List("event"),
        List("contramapM-fn", "contramapM-effect", "sink-fn(7)", "sink-effect(7)")
      ),
      "squelchUnless (passes)" -> Expected(
        List("event"),
        List("p", "sink-fn(7)", "sink-effect(7)")
      ),
      "squelchUnless (filtered)" -> Expected(List("event"), List("p")),
      "traceMaybe (Some)" ->
          Expected(List("event"), List("classify", "sink-fn(7)", "sink-effect(7)")),
      "squelchUnlessM (passes)" ->
          Expected(List("event"), List("pM-fn", "pM-effect", "sink-fn(7)", "sink-effect(7)")),
      "traceMaybeM (None)" -> Expected(List("event"), List("classifyM-fn", "classifyM-effect")),
      "L |+| R" ->
          Expected(List("event", "L-fn(7)"), List("L-effect(7)", "R-fn(7)", "R-effect(7)")),
      "R |+| L" ->
          Expected(List("event", "R-fn(7)"), List("R-effect(7)", "L-fn(7)", "L-effect(7)")),
      "L.contramap |+| R" ->
          Expected(List("event"), List("cmL", "L-fn(7)", "L-effect(7)", "R-fn(7)", "R-effect(7)")),
      "(L |+| R) |+| S" -> Expected(
        List("event", "L-fn(7)"),
        List("L-effect(7)", "R-fn(7)", "R-effect(7)", "S-fn(7)", "S-effect(7)")
      ),
      "L |+| (R |+| S)" -> Expected(
        List("event", "L-fn(7)"),
        List("L-effect(7)", "R-fn(7)", "R-effect(7)", "S-fn(7)", "S-effect(7)")
      ),
      "emit |+| squelched contramap" ->
          Expected(List("event", "sink-fn(7)"), List("sink-effect(7)")),
      "squelched contramap |+| emit" -> Expected(
        List("event"),
        List("sink-fn(7)", "sink-effect(7)")
      ),
      "natTracer(id)" -> Expected(List("event", "sink-fn(7)"), List("sink-effect(7)"))
    )

    /** Trace once, then run the result twice, logging the boundaries between the phases. */
    private def phases[M[_]](r: Runner[M], tracer: ContraTracer[M, Int], log: PhaseLog) = {
        val traced = tracer.traceWith { log("event"); 7 }
        log("returned")
        r.run(traced)
        log("ran once")
        r.run(traced)
        log.drain()
    }

    test("shapes cover the expectations") {
        assert(shapes(Runner.id, PhaseLog()).map(_._1).toSet == expected.keySet)
    }

    test("Id: everything runs at call time, once") {
        val log = PhaseLog()
        shapes(Runner.id, log).foreach { (name, tracer) =>
            val e = expected(name)
            withClue(name) {
                assert(
                  phases(Runner.id, tracer, log) ==
                      e.atCall ++ e.perRun ++ List("returned", "ran once")
                )
            }
        }
    }

    List(Runner.io, Runner.stateT, Runner.kleisli, Runner.eval, Runner.state).foreach { r =>
        test(s"${r.name}: leaves and left legs at call time, shifted stages on every run") {
            val log = PhaseLog()
            shapes(r, log).foreach { (name, tracer) =>
                val e = expected(name)
                withClue(name) {
                    assert(
                      phases(r, tracer, log) ==
                          e.atCall ++ ("returned" :: e.perRun) ++ ("ran once" :: e.perRun)
                    )
                }
            }
        }
    }

    test("building a composed tracer evaluates nothing") {
        val log = PhaseLog()
        List(Runner.id, Runner.io, Runner.eval).foreach(r => shapes(r, log): Unit)
        assert(log.drain() == Nil)
    }

    test("a squelched composition evaluates neither the event nor any stage") {
        val log = PhaseLog()
        val r = Runner.io
        import r.given
        val squelched: List[(String, ContraTracer[cats.effect.IO, Int])] = List(
          "contramap" -> nullTracer.contramap[Int] { i =>
              log("contramap"); i
          },
          "contramapM" -> nullTracer.contramapM[Int] { i =>
              log("contramapM-fn"); r.delay(log("contramapM-effect")).as(i)
          },
          "squelchUnless" -> nullTracer.squelchUnless { _ =>
              log("p"); true
          },
          "traceMaybe" -> nullTracer.traceMaybe[Int] { i =>
              log("classify"); Some(i)
          },
          "squelchUnlessM" -> nullTracer.squelchUnlessM { _ =>
              log("pM-fn"); r.delay(log("pM-effect")).as(true)
          },
          "traceMaybeM" -> nullTracer.traceMaybeM[Int] { i =>
              log("classifyM-fn"); r.delay(log("classifyM-effect")).as(Some(i))
          },
          "|+|" -> (nullTracer[cats.effect.IO, Int].contramap[Int] { i =>
              log("L"); i
          } |+|
              nullTracer[cats.effect.IO, Int].contramap[Int] { i =>
                  log("R"); i
              }),
          "traceAll" -> nullTracer[cats.effect.IO, Int].traceAll[List, Int] { i =>
              log("traceAll"); List(i)
          }
        )
        squelched.foreach { (name, tracer) =>
            withClue(name)(assert(phases(r, tracer, log) == List("returned", "ran once")))
        }
    }
}
