package contratracer

import cats.*
import cats.arrow.*
import cats.syntax.all.*
import contratracer.ContraTracer.nullTracer
import contratracer.TracerA.Squelching

/** A typed, composable contravariant tracer: a component emits typed events and stays unaware of
  * the logging backend, while the caller supplies the functions that format and render those events
  * within an effect monad `M`.
  *
  * A `ContraTracer[M, A]` behaves as `A => M[Unit]`, but is built over the [[TracerA]] arrow, which
  * tags every part as emitting or squelching. That buys two things a plain function can't:
  *   - **Laziness.** A tracer with no emitting part, such as [[ContraTracer.nullTracer]] and
  *     anything composed over it, never builds its event and runs none of its stages. The tag is
  *     fixed when the tracer is built: one emitting part anywhere, including one leg of `|+|`,
  *     makes the whole tracer emitting, and then every event is built.
  *   - **Composition.** Combine sinks with `|+|`, adapt the input with `contramap`, filter with
  *     [[squelchUnless]] or [[traceMaybe]], or change the effect with [[mapK]].
  *
  * A tracer is an immutable value, safe to share across any number of fibers and threads. A sink
  * sees events in the order their `M[Unit]` values run, which can differ from the order of the
  * `traceWith` calls; across fibers, only each fiber's own order holds.
  *
  * Ported to Scala 3 / Cats from Alexander Vieth's Haskell `contra-tracer`
  * ([[https://github.com/avieth/contra-tracer]]), specifically
  * [[https://github.com/avieth/contra-tracer/blob/master/src/Control/Tracer.hs]].
  *
  * @param runTracer
  *   the underlying [[TracerA]] arrow this tracer runs
  * @tparam M
  *   the effect monad events are emitted into
  * @tparam A
  *   the event type this tracer consumes
  */
final class ContraTracer[M[_], A](val runTracer: TracerA[M, A, Unit])(using Monad[M]) {
    import TracerA.given

    /** [[runTracer]] compiled to a plain function, once per tracer. Tracers are wired at
      * construction time and traced hot, and recompiling the arrow per trace allocates; the
      * upstream Haskell gets the same effect from `INLINE traceWith` + GHC let-floating. These are
      * final fields set in the constructor, so a tracer shared across threads is safely published
      * and every trace reads them without synchronisation.
      */
    private val compiled: A => M[Unit] = runTracer.compile

    private val squelching: Boolean = runTracer match {
        case Squelching(_) => true
        case _             => false
    }

    private val squelched: M[Unit] = Monad[M].unit

    /** Trace one event.
      *
      * The event is by-name. A squelching tracer never evaluates it, so building an expensive event
      * costs nothing when nothing would emit it. An emitting tracer evaluates it exactly once, when
      * `traceWith` is called.
      *
      * The call evaluates the event and nothing else. The stages and sinks, in every leg of `|+|`,
      * run when the returned `M[Unit]` runs, and again on each further run. When `M` does not
      * suspend, as with `Id`, that is at the call.
      *
      * Nothing is caught. An event that fails to build is thrown out of `traceWith`. A failing
      * stage or sink fails the returned `M[Unit]` when it runs, the way `M` reports failures: `IO`
      * raises it, `Eval` and `Id` throw it. A failing leg of `|+|` stops the legs after it.
      */
    def traceWith(a: => A): M[Unit] = if squelching then squelched else compiled(a)

    /** Adapts the input with a pure function, run in front of this tracer. The result has this
      * tracer's tag, so `f` never runs in front of a squelching tracer.
      */
    def contramap[B](f: B => A): ContraTracer[M, B] =
        ContraTracer.fromArrow(Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(f) >>> runTracer)

    /** Traces each event into this tracer, then into `that`, and emits if either leg emits. Nothing
      * is caught: if this tracer fails, `that` does not run. `nullTracer` is the identity.
      */
    def |+|(that: ContraTracer[M, A]): ContraTracer[M, A] = {
        val discard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift((_: (Unit, Unit)) => ())
        ContraTracer.fromArrow((runTracer &&& that.runTracer) >>> discard)
    }

    /** A tracer that traces `f(b)` when it is defined, and nothing otherwise.
      *
      * When this tracer squelches, so does the result, and `f` is never called. When this tracer
      * emits, `f` runs on every trace, whatever it returns, because it needs the event to decide.
      */
    def traceMaybe[B](f: B => Option[A]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            Arrow[[X, Y] =>> TracerA[M, X, Y]].lift((x: B) =>
                f(x).map(Right(_)).getOrElse(Left(()))
            )
        ContraTracer.fromArrow(classify >>> (TracerA.squelch ||| runTracer))
    }

    /** [[traceMaybe]] with an effectful classifier. When this tracer emits, `f`'s effect runs on
      * every trace, whether it returns `Some` or `None`; when this tracer squelches, it never runs.
      */
    def traceMaybeM[B](f: B => M[Option[A]]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
        ContraTracer.fromArrow(classify >>> (TracerA.squelch ||| runTracer))
    }

    /** A tracer that traces only the events satisfying `p`.
      *
      * Over an emitting tracer the result still emits, so every event is built and tested by `p`.
      * To silence a tracer entirely, replace it with [[ContraTracer.nullTracer]].
      */
    def squelchUnless(p: (A => Boolean)): ContraTracer[M, A] =
        traceMaybe(a => Some(a).filter(p))

    /** [[squelchUnless]] with an effectful predicate. When this tracer emits, `p`'s effect runs on
      * every trace; when this tracer squelches, it never runs.
      */
    def squelchUnlessM(p: A => M[Boolean]): ContraTracer[M, A] =
        traceMaybeM(a => p(a).map(prop => if prop then Some(a) else None))

    /** A tracer for a whole structure: traces each element through this tracer, in `T`'s fold
      * order.
      *
      * When this tracer squelches, the result is [[ContraTracer.nullTracer]] and the structure is
      * never touched. Otherwise the elements go through `Foldable[T].traverse_` when the returned
      * `M[Unit]` runs, each traced in full before the next. For a strict structure such as `List`
      * or `Vector` in `IO`, the traversal first builds one effect per element and then runs them; a
      * `LazyList` is walked one element at a time.
      */
    def traceTraversable[T[_]: Foldable]: ContraTracer[M, T[A]] =
        if squelching then nullTracer
        else ContraTracer.emit((t: T[A]) => Foldable[T].traverse_(t)(compiled))

    /** Traces every element of `f(b)` through this tracer: [[traceTraversable]] after `f`. When
      * this tracer squelches, `f` is never called.
      */
    def traceAll[T[_]: Foldable, B](f: (B => T[A])): ContraTracer[M, B] =
        traceTraversable[T].contramap(f)

    /** Adapts the input with an effectful function, run in front of this tracer.
      *
      * When this tracer emits, `f`'s effect runs on every trace, even for an event that this tracer
      * then filters out with [[squelchUnless]] or [[traceMaybe]]. When this tracer squelches, `f`
      * never runs.
      */
    def contramapM[B](f: B => M[A]): ContraTracer[M, B] =
        ContraTracer.fromArrow(TracerA.effect(f) >>> runTracer)

    /** Changes the effect with a natural transformation, for instance to use an `IO` tracer inside
      * a monad transformer stack over `IO`.
      *
      * `h` must be a monad morphism, preserving `pure` and `flatMap`. Otherwise the result depends
      * on how this tracer was built: transforming `a |+| b` need not equal combining the
      * transformed `a` and `b`.
      *
      * `h` is applied once each time the returned `N[Unit]` runs, to the whole emitting arrow's
      * result. From a monad that does not suspend, such as `Id` into `IO`, the original sink runs
      * as `h`'s argument, so it too runs each time the `IO` runs. A squelching tracer stays
      * squelching, and `h` is never applied to it.
      */
    def mapK[N[_]: Monad](h: M ~> N): ContraTracer[N, A] =
        ContraTracer.fromArrow(TracerA.nat(h)(runTracer))
}

object ContraTracer {

    /** A tracer over an arrow built with [[TracerA]]'s combinators. */
    def fromArrow[M[_]: Monad, A](runTracer: TracerA[M, A, Unit]): ContraTracer[M, A] =
        new ContraTracer(runTracer)

    /** [[ContraTracer.contramap]], for code generic over `Contravariant`. */
    given [M[_]]: Contravariant[[X] =>> ContraTracer[M, X]] with {
        override def contramap[A, B](tracer: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
            tracer.contramap(f)
    }

    /** [[ContraTracer.|+|]] with [[ContraTracer.nullTracer]] as its identity. */
    given [M[_]: Monad, A]: Monoid[ContraTracer[M, A]] with {
        override def empty: ContraTracer[M, A] = nullTracer

        /** Starts the fold from the first tracer. `Monoid`'s default starts from `nullTracer`,
          * which adds a `|+|` to every trace.
          */
        override def combineAll(as: IterableOnce[ContraTracer[M, A]]): ContraTracer[M, A] =
            combineAllOption(as).getOrElse(empty)

        override def combineAllOption(
            as: IterableOnce[ContraTracer[M, A]]
        ): Option[ContraTracer[M, A]] =
            as.iterator.toVector.reduceRightOption(combine)

        override def combine(x: ContraTracer[M, A], y: ContraTracer[M, A]): ContraTracer[M, A] =
            x |+| y
    }

    /** An emitting tracer that runs `f` on each event; the same as [[emit]]. */
    def apply[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = emit(f)

    /** A tracer that never emits: it builds no event and runs no stage composed over it. It is
      * `Monoid.empty`, the identity of `|+|`.
      *
      * Use it to silence a tracer. `ContraTracer(_ => M.unit)` discards events too, but it is an
      * emitting tracer, so every event is built and every stage in front of it runs.
      */
    def nullTracer[M[_]: Monad, A]: ContraTracer[M, A] = fromArrow(TracerA.squelch)

    /** An emitting tracer that runs `f` on each event. Upstream's name; [[apply]] is the same. */
    def emit[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = fromArrow(TracerA.emit(f))
}
