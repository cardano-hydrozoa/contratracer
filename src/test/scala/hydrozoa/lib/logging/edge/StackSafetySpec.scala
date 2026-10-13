package hydrozoa.lib.logging.edge

import hydrozoa.lib.logging.edge.DeepShapes.*
import org.scalatest.funsuite.AnyFunSuite

/** How deep a composed tracer can be before tracing it overflows a 1 MiB stack (the JVM default
  * thread stack on x86-64 Linux). Each case runs on its own thread with that stack, and an IO runs
  * on that thread through `syncStep`, so an overflow never reaches a runtime worker.
  *
  * Building is iterative for every shape; any overflow happens when `traceWith` is called. The
  * exact first-overflow depth moves with JIT warm-up (`StackDepthProbe` measures it), so these
  * cases assert only depths far from the boundary: 10^3, below every boundary measured, and 10^5,
  * above every one.
  */
class StackSafetySpec extends AnyFunSuite {

    private val stack = 1024L * 1024
    private val shallow = 1000
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
      Shape.ArrowLeft,
      Shape.ArrowRight
    )

    for target <- List(eval, io); shape <- shiftedShapes do {
        test(s"${target.name}: ${shape.label} of depth $deep traces every sink") {
            assert(completes(target, shape, deep))
        }
    }

    /** `|+|` runs its left leg by a direct call, so a left-nested fold recurses once per level at
      * call time, whatever the monad. `combineAll` folds left. A fix moves the left leg behind a
      * `flatMap`, which costs every trace through `|+|`; that is a design decision.
      */
    for target <- List(eval, io); shape <- List(Shape.CombineLeft, Shape.CombineAll) do {
        test(s"${target.name}: ${shape.label} of depth $deep overflows when traced") {
            assert(overflowsAtCall(target, shape, deep))
        }
        test(s"${target.name}: ${shape.label} of depth $deep should trace every sink") {
            pendingUntilFixed(assert(completes(target, shape, deep)): Unit)
        }
    }
}
