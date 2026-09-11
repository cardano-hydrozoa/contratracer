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
  * A `ContraTracer[M, A]` is essentially `A => M[Unit]`, but built over the [[TracerA]] arrow so
  * the emitting / non-emitting structure is preserved. That structure buys two things a plain
  * function can't:
  *   - **Laziness.** A squelching (no-op) tracer skips upstream `contramapM` / predicate / payload
  *     work entirely — the argument is never forced. See [[traceMaybe]].
  *   - **Composition.** Combine sinks with `|+|` (`Semigroup` / `Monoid`), narrow the input with
  *     `contramap`, gate with `squelchUnless`, or swap the effect with a natural transformation via
  *     [[natTracer]].
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
      * upstream Haskell gets the same effect from `INLINE traceWith` + GHC let-floating.
      */
    private lazy val compiled: A => M[Unit] = runTracer.compile

    /** Run a tracer with a given input.
      */
    def traceWith(a: A): M[Unit] = compiled(a)

    /** -- | Inverse of 'arrow'. Useful when writing arrow tracers which use a -- contravariant
      * tracer (the newtype in this module).
      */
    def use: TracerA[M, A, Unit] = runTracer

    /** Run a tracer only for the Just variant of a Maybe. If it's Nothing, the 'nullTracer' is used
      * (no output). The arrow representation allows for proper laziness: if the tracer parameter
      * does not produce any tracing effects, then the predicate won't even be evaluated. Contrast
      * with the simple contravariant representation as
      * @a
      *   -> m ()@, in which the predicate _must_ be forced no matter what, because it's impossible
      *   to know a priori whether that function will not produce any tracing effects.
      */
    def traceMaybe[B](f: B => Option[A]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            Arrow[[X, Y] =>> TracerA[M, X, Y]].lift((x: B) =>
                f(x).map(Right(_)).getOrElse(Left(()))
            )
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
    }

    /** A monadic version of `traceMaybe`.
      */
    def traceMaybeM[B](f: B => M[Option[A]]): ContraTracer[M, B] = {
        def classify: TracerA[M, B, Either[Unit, A]] =
            TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
        ContraTracer(classify >>> (TracerA.squelch ||| this.use))
    }

    /** Uses 'traceMaybe' to give a tracer which emits only if a predicate is true.
      */
    def squelchUnless(p: (A => Boolean)): ContraTracer[M, A] =
        traceMaybe(a => Some(a).filter(p))

    /** A monadic version of `squelchUnless`. */
    def squelchUnlessM(p: A => M[Boolean]): ContraTracer[M, A] =
        traceMaybeM(a => p(a).map(prop => if prop then Some(a) else None))

    def traceTraversable[T[_]: Foldable]: ContraTracer[M, T[A]] = runTracer match {
        case Squelching(_) => nullTracer
        case _ => ContraTracer(TracerA.emit((t: T[A]) => Foldable[T].traverse_(t)(this.traceWith)))
    }

    def traceAll[T[_]: Foldable, B](f: (B => T[A])): ContraTracer[M, B] =
        Contravariant[[X] =>> ContraTracer[M, X]].contramap(this.traceTraversable)(f)

    /** A contravariant transformation of a tracer using a Kleisli arrow
      * @param f
      *   Kleisli arrow which is evaluated only if a downstream tracer emits
      */
    def contramapM[B](f: B => M[A]): ContraTracer[M, B] = ContraTracer(
      TracerA.effect(f) >>> this.use
    )

    /** Use a natural transformation to change the @m@ type. This is useful, for instance, to use
      * concrete IO tracers in monad transformer stacks that have IO as their base.
      */
    def natTracer[N[_]: Monad, S](h: M ~> N): ContraTracer[N, A] = ContraTracer(
      TracerA.nat(h)(this.use)
    )
}

object ContraTracer {
    import TracerA.given

    given [M[_]: Monad]: Contravariant[[X] =>> ContraTracer[M, X]] with {
        override def contramap[A, B](tracer: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
            ContraTracer(Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(f) >>> tracer.use)
    }

    /** tr1.combine(tr2) will run tr1 and then tr2 with the same input.
      */
    given [M[_]: Monad, S]: Semigroup[ContraTracer[M, S]] with {
        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(@unused b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    given [M[_]: Monad, S]: Monoid[ContraTracer[M, S]] with {
        override def empty: ContraTracer[M, S] = nullTracer

        override def combine(x: ContraTracer[M, S], y: ContraTracer[M, S]): ContraTracer[M, S] = {
            def const[A, B](a: A)(@unused b: => B): A = a
            def discard(tunit: (Unit, Unit)): Unit = const(())(tunit)
            def arrDiscard = Arrow[[X, Y] =>> TracerA[M, X, Y]].lift(discard)
            ContraTracer((x.use &&& y.use) >>> arrDiscard)
        }
    }

    /** Make an emitting tracer from a callback. -- mkTracer :: Applicative m => (a -> m ()) ->
      * Tracer m a mkTracer = Tracer . Arrow.emit
      */
    def apply[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )

    /** A tracer which does nothing. nullTracer :: Monad m => Tracer m a nullTracer = Tracer
      * Arrow.squelch
      */
    def nullTracer[M[_]: Monad, A]: ContraTracer[M, A] = ContraTracer(TracerA.squelch)

    /** Create a simple contravariant tracer which runs a given side-effect. emit :: Applicative m =>
      * (a -> m ()) -> Tracer m a emit f = Tracer (Arrow.emit f)
      */
    def emit[M[_]: Monad, A](f: A => M[Unit]): ContraTracer[M, A] = ContraTracer(
      TracerA.emit(f)
    )
}

/** Syntax for using an ambient ContraTracer from a `given`
  */
object ContraTracerSyntax {

    def traceWith[M[_], A](a: A)(using ct: ContraTracer[M, A]): M[Unit] =
        ct.traceWith(a)

    def use[M[_], A](using ct: ContraTracer[M, A]): TracerA[M, A, Unit] = ct.runTracer

    def traceMaybe[M[_], A, B](f: B => Option[A])(using
        ct: ContraTracer[M, A]
    ): ContraTracer[M, B] =
        ct.traceMaybe(f)
//
//        /** A monadic version of `traceMaybe`.
//         */
//        def traceMaybeM[B](f: B => M[Option[A]]): ContraTracer[M, B] = {
//            def classify: TracerA[M, B, Either[Unit, A]] =
//                TracerA.effect((x: B) => f(x).map(_.map(Right(_)).getOrElse(Left(()))))
//
//            ContraTracer(classify >>> (TracerA.squelch ||| this.use))
//        }
//
//        /** Uses 'traceMaybe' to give a tracer which emits only if a predicate is true.
//         */
//        def squelchUnless(p: (A => Boolean)): ContraTracer[M, A] =
//            traceMaybe(a => Some(a).filter(p))
//
//        /** A monadic version of `squelchUnless`. */
//        def squelchUnlessM(p: A => M[Boolean]): ContraTracer[M, A] =
//            traceMaybeM(a => p(a).map(prop => if prop then Some(a) else None))
//
//        def traceTraversable[T[_] : Foldable](using Monad[M]): ContraTracer[M, T[A]] = runTracer match {
//            case Squelching(_) => nullTracer
//            case _ => ContraTracer(TracerA.emit((t: T[A]) => Foldable[T].traverse_(t)(this.traceWith)))
//        }
//
//        def traceAll[T[_] : Foldable, B](f: (B => T[A]))(using Monad[M]): ContraTracer[M, B] =
//            Contravariant[[X] =>> ContraTracer[M, X]].contramap(this.traceTraversable)(f)
//
//        /** A contravariant transformation of a tracer using a Kleisli arrow
//         *
//         * @param f
//         * Kleisli arrow which is evaluated only if a downstream tracer emits
//         */
//        def contramapM[B](f: B => M[A]): ContraTracer[M, B] = ContraTracer(
//            TracerA.effect(f) >>> this.use
//        )
//
//        /** Use a natural transformation to change the @m@ type. This is useful, for instance, to use
//         * concrete IO tracers in monad transformer stacks that have IO as their base.
//         */
//        def natTracer[N[_], S](h: M ~> N) = ContraTracer(TracerA.nat(h)(this.use))
//    }
}
