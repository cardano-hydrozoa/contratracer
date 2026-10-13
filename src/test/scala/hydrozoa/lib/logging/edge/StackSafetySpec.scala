package hydrozoa.lib.logging.edge

import hydrozoa.lib.logging.edge.DeepShapes.*
import org.scalatest.funsuite.AnyFunSuite

/** How deep a composed tracer can be before tracing it overflows a 1 MiB stack (the JVM default
  * thread stack on x86-64 Linux). Each case runs on its own thread with that stack, and an IO runs
  * on that thread through `syncStep`, so an overflow never reaches a runtime worker.
  *
  * Building is iterative for every shape; any overflow happens when `traceWith` is called. The
  * first-overflow depth moves with JIT warm-up (`StackDepthProbe` measures it): interpreted frames
  * are several times larger than compiled ones, and a cold `Id` chain of `squelchUnless` can
  * overflow near depth 10^3. So these cases assert only depths far from every boundary: 10^2 and
  * 10^5.
  */
class StackSafetySpec extends AnyFunSuite {

    private val stack = 1024L * 1024
    private val shallow = 100
    private val deep = 100000

    private def completes[M[_]](target: Target[M], shape: Shape, depth: Int): Boolean =
        measure(target, shape, depth, stack) == Outcome.Completed(expectedEmissions(shape, depth))

    private def overflowsAtCall[M[_]](target: Target[M], shape: Shape, depth: Int): Boolean =
        measure(target, shape, depth, stack) == Outcome.Overflowed(Phase.Call)

    for target <- targets; shape <- Shape.values do {
        test(s"${target.name}: ${shape.label} of depth $shallow traces every sink") {
            assert(completes(target, shape, shallow))
        }
    }

    for shape <- Shape.values do {
        test(s"Id: ${shape.label} of depth $deep overflows when traced") {
            assert(overflowsAtCall(id, shape, deep))
        }
    }

    private val shiftedShapes = List(
      Shape.ContramapChain,
      Shape.SquelchUnlessChain,
      Shape.CombineRight,
      Shape.CombineAll,
      Shape.ArrowLeft,
      Shape.ArrowRight
    )

    for target <- List(eval, io); shape <- shiftedShapes do {
        test(s"${target.name}: ${shape.label} of depth $deep traces every sink") {
            assert(completes(target, shape, deep))
        }
    }

    /** `|+|` runs its left leg by a direct call, so a left-nested fold recurses once per level at
      * call time, whatever the monad. A fix moves the left leg behind a `flatMap`, which costs
      * every trace through `|+|`; that is a design decision.
      */
    for target <- List(eval, io); shape <- List(Shape.CombineLeft) do {
        test(s"${target.name}: ${shape.label} of depth $deep overflows when traced") {
            assert(overflowsAtCall(target, shape, deep))
        }
        test(s"${target.name}: ${shape.label} of depth $deep should trace every sink") {
            pendingUntilFixed(assert(completes(target, shape, deep)): Unit)
        }
    }

    test("Monoid.combineAll nests right and keeps the order the tracers were given in") {
        import cats.Monoid
        import hydrozoa.lib.logging.ContraTracer
        val seen = scala.collection.mutable.ListBuffer.empty[Int]
        val legs = (1 to 5).toList.map(i => ContraTracer.emit[cats.Id, Int](_ => seen += i: Unit))
        Monoid[ContraTracer[cats.Id, Int]].combineAll(legs).traceWith(0)
        assert(seen.toList == List(1, 2, 3, 4, 5))
    }
}
