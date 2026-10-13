package hydrozoa.lib.logging

import cats.*
import cats.arrow.*
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer.nullTracer
import hydrozoa.lib.logging.TracerA.Squelching
import scala.annotation.unused

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
  *     [[squelchUnless]] or [[traceMaybe]], or change the effect with [[natTracer]].
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
case class ContraTracer[M[_], A](runTracer: TracerA[M, A, Unit])(using Monad[M]) {
    import TracerA.given
    import ContraTracer.given

    /** [[runTracer]] compiled to a plain function, once per tracer. Tracers are wired at
      * construction time and traced hot, and recompiling the arrow per trace allocates; the
      * upstream Haskell gets the same effect from `INLINE traceWith` + GHC let-floating. A Scala 3
      * lazy val is initialised once even when threads race to the first trace, which is what makes
      * a shared tracer safe.
      */
    private lazy val compiled: A => M[Unit] = runTracer.compile

    /** Trace one event.
      *
      * The event is by-name. A squelching tracer never evaluates it, so building an expensive event
      * costs nothing when nothing would emit it. An emitting tracer evaluates it exactly once, when
      * `traceWith` is called rather than when the returned `M[Unit]` runs.
      *
      * When `M` suspends (`IO`, `Eval`, `State`), the call also runs the function given to an
      * [[ContraTracer.emit]] leaf with nothing composed in front of it: the tracer itself, or the
      * leftmost leg of `|+|`. Everything else runs when the returned `M[Unit]` runs, and again on
      * each further run: every stage composed in front of a sink (`contramap`, `contramapM`,
      * `squelchUnless`, `traceMaybe` and their `M` forms) and every later leg of `|+|`. In `Id`,
      * everything runs at the call.
      *
      * Nothing is caught. A failure in a part that runs at the call is thrown out of `traceWith`; a
      * failure in a part that runs later fails the returned `M[Unit]`. A failing leg of `|+|` stops
      * the legs after it.
      */
    def traceWith(a: => A): M[Unit] = runTracer match {
        case Squelching(_) => squelched
        case _             => compiled(a)
    }

    private lazy val squelched: M[Unit] = Monad[M].unit

    /** The underlying arrow, the same value as `runTracer`, under upstream's name. Use it to embed
      * this tracer in a larger [[TracerA]] expression.
      */
    def use: TracerA[M, A, Unit] = runTracer

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
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
    }

    /** [[traceMaybe]] with an effectful classifier. When this tracer emits, `f`'s effect runs on
      * every trace, whether it returns `Some` or `None`; when this tracer squelches, it never runs.
      */
    def traceMaybeM[B](f: B => M[Option[A]]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
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
      * never touched. Otherwise the elements go through `Foldable[T].traverse_`, so `T` decides
      * when each is visited. For a strict structure such as `List` or `Vector` in a suspending
      * monad, the call-time part of this tracer (see [[traceWith]]) runs for every element before
      * `traceWith` returns, and the returned `M[Unit]` holds one effect per element. A `LazyList`
      * is walked as the returned `M[Unit]` runs.
      */
    def traceTraversable[T[_]: Foldable]: ContraTracer[M, T[A]] = runTracer match {
        case Squelching(_) => nullTracer
        case _ => ContraTracer(TracerA.emit((t: T[A]) => Foldable[T].traverse_(t)(compiled)))
    }

    /** Traces every element of `f(b)` through this tracer: [[traceTraversable]] after `f`. When
      * this tracer squelches, `f` is never called.
      */
    def traceAll[T[_]: Foldable, B](f: (B => T[A])): ContraTracer[M, B] =
        Contravariant[[X] =>> ContraTracer[M, X]].contramap(this.traceTraversable)(f)

    /** Adapts the input with an effectful function, run in front of this tracer.
      *
      * When this tracer emits, `f`'s effect runs on every trace, even for an event that this tracer
      * then filters out with [[squelchUnless]] or [[traceMaybe]]. When this tracer squelches, `f`
      * never runs.
      */
    def contramapM[B](f: B => M[A]): ContraTracer[M, B] = ContraTracer(
      TracerA.effect(f) >>> this.use
    )

    /** Changes the effect with a natural transformation, for instance to use an `IO` tracer inside
      * a monad transformer stack over `IO`.
      *
      * `h` must be a monad morphism, preserving `pure` and `flatMap`. Otherwise the result depends
      * on how this tracer was built: transforming `a |+| b` need not equal combining the
      * transformed `a` and `b`.
      *
      * `h` is applied once per trace, with the timing of an `emit` leaf's function (see
      * [[traceWith]]): at the call, unless a stage is composed in front of the result. From a monad
      * that does not suspend, such as `Id` into `IO`, the original sink therefore runs at the call,
      * and running the result does not run it again. A squelching tracer stays squelching, and `h`
      * is never applied to it.
      */
    def natTracer[N[_]: Monad, S](h: M ~> N): ContraTracer[N, A] = ContraTracer(
      TracerA.nat(h)(this.use)
    )
}

object ContraTracer {
    import TracerA.given

    /** `contramap`: adapts the input with a pure function, run in front of the tracer. The result
      * has the tracer's tag, so `f` never runs in front of a squelching tracer.
      */
    given [M[_]: Monad]: Contravariant[[X] =>> ContraTracer[M, X]] with {
        override def contramap[A, B](tracer: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
            ContraTracer(Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(f) >>> tracer.use)
    }

    /** `x |+| y` traces each event into `x`, then into `y`, and emits if either leg emits. Nothing
      * is caught: if `x` fails, `y` does not run. The same `combine` as the `Monoid` given.
      */
    given [M[_]: Monad, S]: Semigroup[ContraTracer[M, S]] with {
        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(@unused b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    /** `|+|` with [[ContraTracer.nullTracer]] as its identity. `x |+| y` traces each event into
      * `x`, then into `y`, and emits if either leg emits. Nothing is caught: if `x` fails, `y` does
      * not run.
      */
    given [M[_]: Monad, S]: Monoid[ContraTracer[M, S]] with {
        override def empty: ContraTracer[M, S] = nullTracer

        /** Nests right. `combine` runs its left tracer by a direct call when traced and its right
          * one inside `flatMap`, so tracing a left-nested fold recurses once per tracer in any
          * monad, while a right-nested one recurses only in a monad whose `flatMap` does. Fold
          * tracers with this rather than a hand-written left fold of `|+|`.
          */
        override def combineAll(as: IterableOnce[ContraTracer[M, S]]): ContraTracer[M, S] =
            combineAllOption(as).getOrElse(empty)

        override def combineAllOption(
            as: IterableOnce[ContraTracer[M, S]]
        ): Option[ContraTracer[M, S]] =
            as.iterator.toVector.reduceRightOption(combine)

        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(@unused b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    /** An emitting tracer that runs `f` on each event; the same as [[emit]]. */
    def apply[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )

    /** A tracer that never emits: it builds no event and runs no stage composed over it. It is
      * `Monoid.empty`, the identity of `|+|`.
      *
      * Use it to silence a tracer. `ContraTracer(_ => M.unit)` discards events too, but it is an
      * emitting tracer, so every event is built and every stage in front of it runs.
      */
    def nullTracer[M[_]: Monad, A]: ContraTracer[M, A] = ContraTracer(TracerA.squelch)

    /** An emitting tracer that runs `f` on each event. Upstream's name; [[apply]] is the same.
      *
      * `f` itself can run when `traceWith` is called (see [[ContraTracer.traceWith]]), so keep it
      * cheap and put the work in the `M[Unit]` it returns.
      */
    def emit[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )
}

/** Syntax for tracing through a `ContraTracer[M, A]` given in scope.
  *
  * The given is found by its exact type, because `A` is invariant. When the event's static type is
  * a subtype of `A`, such as a case class extending a sealed trait, pass the type arguments.
  */
object ContraTracerSyntax {

    /** [[ContraTracer.traceWith]] on the given tracer; the event is by-name here too. */
    def traceWith[M[_], A](a: => A)(using ct: ContraTracer[M, A]): M[Unit] =
        ct.traceWith(a)

    def use[M[_], A](using ct: ContraTracer[M, A]): TracerA[M, A, Unit] = ct.runTracer

    def traceMaybe[M[_], A, B](f: B => Option[A])(using
        ct: ContraTracer[M, A]
    ): ContraTracer[M, B] =
        ct.traceMaybe(f)
}
