package contratracer.edge

import cats.arrow.FunctionK
import cats.syntax.all.*
import contratracer.ContraTracer
import contratracer.ContraTracer.nullTracer
import org.scalatest.funsuite.AnyFunSuite

/** When each part of a composed tracer is evaluated, relative to `traceWith` returning and to the
  * returned `M[Unit]` being run, in each monad.
  *
  * `traceWith` evaluates the event and nothing else. The compiled tracer runs its emitting part
  * behind a `flatMap(unit)`, so in a suspending monad every stage and every sink, in every leg of
  * `|+|`, runs when the result is run, and again on every further run. In `Id` there is no
  * difference: everything runs at call time, once.
  */
class EvaluationTimingSpec extends AnyFunSuite {

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
          "mapK(id)" -> leaf("sink").mapK(FunctionK.id[M])
        )
    }

    /** What a shape logs on each run in a suspending monad, after the event at call time. */
    private val perRun: Map[String, List[String]] = Map(
      "emit" -> List("sink-fn(7)", "sink-effect(7)"),
      "contramap" -> List("contramap", "sink-fn(8)", "sink-effect(8)"),
      "contramap twice" -> List("outer", "inner", "sink-fn(9)", "sink-effect(9)"),
      "contramapM" ->
          List("contramapM-fn", "contramapM-effect", "sink-fn(7)", "sink-effect(7)"),
      "squelchUnless (passes)" -> List("p", "sink-fn(7)", "sink-effect(7)"),
      "squelchUnless (filtered)" -> List("p"),
      "traceMaybe (Some)" -> List("classify", "sink-fn(7)", "sink-effect(7)"),
      "squelchUnlessM (passes)" -> List("pM-fn", "pM-effect", "sink-fn(7)", "sink-effect(7)"),
      "traceMaybeM (None)" -> List("classifyM-fn", "classifyM-effect"),
      "L |+| R" -> List("L-fn(7)", "L-effect(7)", "R-fn(7)", "R-effect(7)"),
      "R |+| L" -> List("R-fn(7)", "R-effect(7)", "L-fn(7)", "L-effect(7)"),
      "L.contramap |+| R" -> List("cmL", "L-fn(7)", "L-effect(7)", "R-fn(7)", "R-effect(7)"),
      "(L |+| R) |+| S" ->
          List("L-fn(7)", "L-effect(7)", "R-fn(7)", "R-effect(7)", "S-fn(7)", "S-effect(7)"),
      "L |+| (R |+| S)" ->
          List("L-fn(7)", "L-effect(7)", "R-fn(7)", "R-effect(7)", "S-fn(7)", "S-effect(7)"),
      "emit |+| squelched contramap" -> List("sink-fn(7)", "sink-effect(7)"),
      "squelched contramap |+| emit" -> List("sink-fn(7)", "sink-effect(7)"),
      "mapK(id)" -> List("sink-fn(7)", "sink-effect(7)")
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
        assert(shapes(Runner.id, PhaseLog()).map(_._1).toSet == perRun.keySet)
    }

    test("Id: everything runs at call time, once") {
        val log = PhaseLog()
        shapes(Runner.id, log).foreach { (name, tracer) =>
            withClue(name) {
                assert(
                  phases(Runner.id, tracer, log) ==
                      "event" :: perRun(name) ++ List("returned", "ran once")
                )
            }
        }
    }

    List(Runner.io, Runner.stateT, Runner.kleisli, Runner.eval, Runner.state).foreach { r =>
        test(s"${r.name}: only the event at call time, every stage and sink on every run") {
            val log = PhaseLog()
            shapes(r, log).foreach { (name, tracer) =>
                val run = perRun(name)
                withClue(name) {
                    assert(
                      phases(r, tracer, log) ==
                          List("event", "returned") ++ run ++ ("ran once" :: run)
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
