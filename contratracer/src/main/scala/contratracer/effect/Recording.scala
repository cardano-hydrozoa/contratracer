package contratracer.effect

import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all.*
import contratracer.ContraTracer

/** Tracers that keep every event they are given, for tests.
  *
  * The events live in a `Ref`, so any number of fibers may trace at once and none is lost. Each
  * fiber's events keep the order in which their traces ran; across fibers the order is the
  * scheduler's. Under `TestControl` one seed always gives the same order and another seed may not,
  * so assert each fiber's own order, or membership, unless the seed is fixed.
  */
object Recording {

    /** A recording tracer, and the effect that reads every event it has recorded so far, oldest
      * first.
      */
    def make[F[_], A](using F: Concurrent[F]): F[(ContraTracer[F, A], F[Vector[A]])] =
        F.ref(Vector.empty[A]).map(events => (record(events), events.get))

    /** [[make]], with a third effect that waits for the first event satisfying `p` and returns it.
      *
      * The event is recorded before the waiter wakes, so a read after the wait contains it. Later
      * events that satisfy `p` are recorded and change nothing else. `p` runs on every event, when
      * the traced effect runs.
      */
    def awaiting[F[_], A](p: A => Boolean)(using
        F: Concurrent[F]
    ): F[(ContraTracer[F, A], F[Vector[A]], F[A])] =
        (F.ref(Vector.empty[A]), F.deferred[A]).mapN { (events, first) =>
            val signal =
                ContraTracer.emit[F, A](a => if p(a) then first.complete(a).void else F.unit)
            (record(events) |+| signal, events.get, first.get)
        }

    private def record[F[_]: Concurrent, A](events: Ref[F, Vector[A]]): ContraTracer[F, A] =
        ContraTracer.emit[F, A](a => events.update(_ :+ a))
}
