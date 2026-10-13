package contratracer.effect

import cats.effect.kernel.{MonadCancel, Outcome}
import contratracer.ContraTracer

/** Tracing around an act whose consequence can cancel the tracing fiber, such as replying to a peer
  * that then hangs up. A trace placed after such an act may never run.
  */
object TraceAroundAct {

    /** Traces `event`, then runs `act`, as one uncancelable step: `event` is traced if and only if
      * `act` runs.
      *
      * The event is built each time the returned effect runs. A cancel that arrives during the
      * trace or between the trace and the act waits until the act completes, so `act` should be
      * short, such as completing a `Deferred` or sending one message.
      */
    def traceThenAct[F[_], E, A, B](tracer: ContraTracer[F, A])(event: => A)(act: F[B])(using
        F: MonadCancel[F, E]
    ): F[B] =
        F.uncancelable(_ => F.productR(F.flatMap(F.unit)(_ => tracer.traceWith(event)))(act))

    /** Runs `act`, then traces how it ended, in a finaliser that a cancel cannot skip.
      *
      * `event` receives the outcome: success, error or cancellation. The trace follows the act, so
      * an observer of the act can see it before the trace is written. A cancel that lands inside
      * `act` after its visible effect reports `Canceled`, so the event for `Canceled` must not
      * claim the act had no effect.
      */
    def actThenTraceOutcome[F[_], E, A, B](tracer: ContraTracer[F, A])(act: F[B])(using
        F: MonadCancel[F, E]
    )(event: Outcome[F, E, B] => A): F[B] =
        F.guaranteeCase(act)(outcome => tracer.traceWith(event(outcome)))
}
