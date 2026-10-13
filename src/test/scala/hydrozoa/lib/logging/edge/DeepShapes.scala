package hydrozoa.lib.logging.edge

import cats.arrow.Arrow
import cats.effect.IO
import cats.syntax.all.*
import cats.{Eval, Id, Monad, Monoid}
import hydrozoa.lib.logging.{ContraTracer, TracerA}

/** Tracers composed `depth` levels deep, and a way to build, trace and run one on a thread with a
  * chosen stack size, reporting the phase that overflowed.
  */
object DeepShapes {

    enum Phase { case Build, Call, Run }

    enum Outcome {
        case Completed(emissions: Long)
        case Overflowed(phase: Phase)
    }

    enum Shape(val label: String) {
        case ContramapChain extends Shape("contramap chain")
        case SquelchUnlessChain extends Shape("squelchUnless chain")
        case CombineLeft extends Shape("|+| left-nested")
        case CombineRight extends Shape("|+| right-nested")
        case CombineAll extends Shape("Monoid.combineAll")
        case ArrowLeft extends Shape(">>> left-nested")
        case ArrowRight extends Shape(">>> right-nested")
    }

    /** The emissions one trace of `shape` at `depth` makes. */
    def expectedEmissions(shape: Shape, depth: Int): Long = shape match {
        case Shape.CombineLeft | Shape.CombineRight => depth.toLong + 1
        case Shape.CombineAll                       => depth.toLong
        case _                                      => 1L
    }

    def build[M[_]: Monad](shape: Shape, depth: Int, sink: Int => M[Unit]): ContraTracer[M, Int] = {
        import TracerA.given
        def leaf = ContraTracer.emit[M, Int](sink)
        def step = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift((i: Int) => i + 1)
        def idA = Arrow[[X, Y] =>> TracerA[M, X, Y]].id[Int]
        shape match {
            case Shape.ContramapChain =>
                (1 to depth).foldLeft(leaf)((t, _) => t.contramap[Int](_ + 1))
            case Shape.SquelchUnlessChain =>
                (1 to depth).foldLeft(leaf)((t, _) => t.squelchUnless(_ => true))
            case Shape.CombineLeft  => (1 to depth).foldLeft(leaf)((t, _) => t |+| leaf)
            case Shape.CombineRight => (1 to depth).foldLeft(leaf)((t, _) => leaf |+| t)
            case Shape.CombineAll =>
                Monoid[ContraTracer[M, Int]].combineAll(Iterator.fill(depth)(leaf))
            case Shape.ArrowLeft =>
                ContraTracer((1 to depth).foldLeft(idA)((t, _) => t >>> step) >>> leaf.use)
            case Shape.ArrowRight =>
                ContraTracer((1 to depth).foldLeft(leaf.use)((t, _) => step >>> t))
        }
    }

    /** One monad: how to make a counting sink and how to run a traced value on this thread. */
    trait Target[M[_]] {
        def name: String
        def monad: Monad[M]
        def sink(count: () => Unit): Int => M[Unit]
        def runHere(m: M[Unit]): Unit
    }

    val id: Target[Id] = new Target[Id] {
        val name = "Id"
        val monad: Monad[Id] = summon[Monad[Id]]
        def sink(count: () => Unit): Int => Id[Unit] = _ => count()
        def runHere(m: Id[Unit]): Unit = m
    }

    val eval: Target[Eval] = new Target[Eval] {
        val name = "Eval"
        val monad: Monad[Eval] = summon[Monad[Eval]]
        def sink(count: () => Unit): Int => Eval[Unit] = _ => Eval.always(count())
        def runHere(m: Eval[Unit]): Unit = m.value
    }

    /** IO's interpreter runs on the calling thread through `syncStep`, so its stack is the probe
      * thread's and an overflow cannot reach a runtime worker.
      */
    val io: Target[IO] = new Target[IO] {
        val name = "IO"
        val monad: Monad[IO] = summon[Monad[IO]]
        def sink(count: () => Unit): Int => IO[Unit] = _ => IO.delay(count())
        def runHere(m: IO[Unit]): Unit =
            m.syncStep(Int.MaxValue).unsafeRunSync() match {
                case Right(())  => ()
                case Left(rest) => sys.error(s"traced IO went async: $rest")
            }
    }

    val targets: List[Target[?]] = List(id, eval, io)

    /** Build, trace once and run, on a fresh thread of `stackBytes` (0 for the JVM default). */
    def measure[M[_]](target: Target[M], shape: Shape, depth: Int, stackBytes: Long): Outcome = {
        given Monad[M] = target.monad
        @volatile var outcome: Outcome = Outcome.Overflowed(Phase.Build)
        @volatile var failure: Throwable = null
        val body: Runnable = () => {
            var emissions = 0L
            var phase = Phase.Build
            try {
                val tracer = build(shape, depth, target.sink(() => emissions += 1))
                phase = Phase.Call
                val traced = tracer.traceWith(1)
                phase = Phase.Run
                target.runHere(traced)
                outcome = Outcome.Completed(emissions)
            } catch {
                case _: StackOverflowError => outcome = Outcome.Overflowed(phase)
                case e: Throwable          => failure = e
            }
        }
        val thread = new Thread(null, body, s"stack-probe-${target.name}", stackBytes)
        thread.start()
        thread.join()
        if failure != null then throw failure
        outcome
    }

    /** The smallest depth in `[1, cap]` whose trace overflows, and the phase it overflowed in, by
      * doubling then bisection. The JIT changes frame sizes as it warms, so the boundary moves
      * between runs; a depth near it may overflow once and complete the next time.
      */
    def firstOverflow[M[_]](
        target: Target[M],
        shape: Shape,
        stackBytes: Long,
        cap: Int
    ): Option[(Int, Phase)] = {
        def overflow(depth: Int): Option[Phase] = measure(target, shape, depth, stackBytes) match {
            case Outcome.Overflowed(phase) => Some(phase)
            case Outcome.Completed(_)      => None
        }
        val doubling = Iterator.iterate(1)(d => (d * 2).min(cap)).map(d => (d, overflow(d)))
        doubling.find((d, phase) => phase.isDefined || d == cap).flatMap {
            case (_, None) => None
            case (hi0, Some(phase0)) =>
                var (lo, hi, phase) = (hi0 / 2, hi0, phase0)
                while hi - lo > 1 do {
                    val mid = lo + (hi - lo) / 2
                    overflow(mid) match {
                        case Some(p) => hi = mid; phase = p
                        case None    => lo = mid
                    }
                }
                Some((hi, phase))
        }
    }
}
