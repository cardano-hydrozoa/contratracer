package contratracer

import cats.*
import cats.arrow.*
import cats.data.*
import cats.syntax.all.*
import scala.annotation.unused

// Ported to Scala 3 / Cats from Alexander Vieth's Haskell `contra-tracer`, chiefly
// `src/Control/Tracer/Arrow.hs`: https://github.com/avieth/contra-tracer
// Modified from the original — see NOTICE. The commented-out Haskell below quotes the upstream
// declarations the Scala translates.

//-- | Formal representation of a tracer arrow as a Kleisli arrow over some
//    -- monad, but tagged so that we know whether it has any effects which will emit
//    -- a trace.
//    data TracerA m a b where
//    -- | An emitting part, followed by a non-emitting part.
//-- The non-emitting part is there so that later emitting parts can be
//    -- tacked-on later.
//Emitting   :: Kleisli m a x -> Kleisli m x b -> TracerA m a b
//    -- | No emitting. There may be side-effects, but they are assumed to be
//    -- benign and will be discarded by 'runTracerA'.
//Squelching :: Kleisli m a b                  -> TracerA m a b

/** A tracer arrow from `A` to `B` in `M`: a Kleisli arrow tagged with whether it emits a trace.
  *
  * Emitting effects produce the trace. Non-emitting effects run only when an emitting effect
  * follows them, and are dropped otherwise, so they must be benign: they compute what a later
  * emitting effect needs. A composite emits if any of its parts emits.
  *
  * [[ContraTracer]] wraps a `TracerA[M, A, Unit]`, and [[compile]] turns one into the function that
  * `traceWith` applies.
  */
sealed trait TracerA[M[_], A, B]

private def const[A, B](a: A)(@unused b: => B): A = a

private val discardResult: Any => Unit = _ => ()

extension [M[_], A](t: TracerA[M, A, Unit])

    // runTracerA :: Monad m => TracerA m a () -> Kleisli m a ()
    // runTracerA (Emitting emits _noEmits) = emits >>> arr (const ())
    // runTracerA (Squelching     _       ) =           arr (const ())

    /** The emitting part of this tracer as a Kleisli arrow, with its result discarded: every effect
      * needed to emit, and none after the last emitting one. A squelching tracer gives a constant
      * `unit`. [[compile]] is the same as a plain function.
      */
    def runTracerA(using monadM: Monad[M]): Kleisli[M, A, Unit] = {
        def arrConst = Kleisli(x => monadM.pure(const(())(x)))
        t match {
            case TracerA.Emitting(emits, _) => emits >>> arrConst
            case TracerA.Squelching(_)      => arrConst

        }
    }

    /** [[runTracerA]] compiled to a plain function, built once per tracer rather than per trace.
      * The upstream Haskell gets this for free — `traceWith` is INLINE and `arr (const ())` is a
      * CAF, so GHC floats the compiled arrow out of the hot path; on the JVM the equivalent must be
      * done by hand ([[ContraTracer]] memoises the result). A squelching tracer compiles to a
      * constant `unit`, making a squelched `traceWith` allocation-free.
      *
      * The emitting case discards the result with `map`, never `void`: a monad may implement `void`
      * without running its argument (cats' `Eval` returns `Eval.Unit`), which would drop every
      * emission. For `IO`, `void` is this same `map`.
      */
    def compile(using monadM: Monad[M]): A => M[Unit] = t match {
        case TracerA.Emitting(emits, _) => a => monadM.map(emits.run(a))(discardResult)
        case TracerA.Squelching(_) =>
            val unit = monadM.unit
            _ => unit
    }

object TracerA:
    /** An emitting part followed by a non-emitting part.
      *
      * @param emitter
      *   everything up to and including the last emitting effect; [[compile]] runs it
      * @param nonEmitter
      *   the stages after the last emitting effect. When an emitting stage is composed after this
      *   arrow, they join its emitter and run; otherwise they are dropped.
      */
    case class Emitting[M[_], A, X, B](emitter: Kleisli[M, A, X], nonEmitter: Kleisli[M, X, B])
        extends TracerA[M, A, B]

    /** An arrow with no emitting effect. Its stages run only when an emitting stage is composed
      * after it; otherwise they are dropped, and a `ContraTracer` over it traces nothing.
      */
    case class Squelching[M[_], A, B](squelching: Kleisli[M, A, B]) extends TracerA[M, A, B]

    /** Ignores the input and emits nothing: the arrow behind [[ContraTracer.nullTracer]]. */
    def squelch[M[_]: Applicative, A]: TracerA[M, A, Unit] = compute(_ => ())

    /** An emitting effect: the arrow emits. Contrast [[effect]]. */
    def emit[M[_], A](f: A => M[Unit])(using appM: Applicative[M]): TracerA[M, A, Unit] = Emitting(
      emitter = Kleisli(f),
      nonEmitter = Kleisli(const(appM.unit))
    )

    /** A non-emitting effect. It runs only when an emitting stage is composed after it, and is
      * dropped otherwise. Contrast [[emit]].
      */
    def effect[M[_], A, B](f: A => M[B]): TracerA[M, A, B] = Squelching(Kleisli(f))

    /** A pure function as a non-emitting stage. */
    def compute[M[_], A, B](f: A => B)(using appM: Applicative[M]): TracerA[M, A, B] =
        effect(x => appM.pure(f(x)))

    private object TracerAOps {
        def compose[M[_]: Monad, A, B, C](
            f: TracerA[M, B, C],
            g: TracerA[M, A, B]
        ): TracerA[M, A, C] =
            (f, g) match {
                case (Squelching(l), Squelching(r)) => Squelching(r.andThen(l))

                case (Squelching(l), Emitting(re, rp)) =>
                    // Keep the non-emitting parts together. `Emitting(re.andThen(rp), l)` is also
                    // correct, but would run `rp` on every trace instead of leaving it droppable.
                    Emitting(re, rp.andThen(l))

                case (Emitting(le, lp), Squelching(r)) =>
                    // The non-emitting part runs first here, so it must run whenever the emitter
                    // does: it joins the emitter.
                    Emitting(r.andThen(le), lp)
                case (Emitting(le, lp), Emitting(re, rp)) =>
                    Emitting(re.andThen(rp).andThen(le), lp)
            }

        def lift[M[_]: Monad, A, B](f: A => B): TracerA[M, A, B] = compute[M, A, B](f)

        // Upstream defines no `first`; Haskell derives `first f = f *** id`, which on each
        // constructor is exactly this (Kleisli's `first` carries the `C` through unchanged).
        def first[M[_]: Monad, A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = {
            fa match {
                case Squelching(s)  => Squelching(s.first)
                case Emitting(e, p) => Emitting(e.first, p.first)
            }
        }

        // Strict Kleisli combinators backing the split/merge/choice overrides below — direct
        // translations of the upstream Haskell's hand-written (****)/(|||) equations. The class
        // defaults route through `first` plus lifted swap/dup/fold stages, each of which survives
        // as a per-trace pure+flatMap in the fused arrow (upstream measured the default (***) at
        // ~1KB of thunks per level per dispatch); these run the two legs directly.
        def idK[M[_], A](using appM: Applicative[M]): Kleisli[M, A, A] =
            Kleisli(a => appM.pure(a))

        /** `f **** g`: run both legs on the halves of a pair. */
        def parK[M[_], A, B, C, D](f: Kleisli[M, A, B], g: Kleisli[M, C, D])(using
            monadM: Monad[M]
        ): Kleisli[M, (A, C), (B, D)] =
            Kleisli((ac: (A, C)) =>
                monadM.flatMap(f.run(ac._1))(b => monadM.map(g.run(ac._2))(d => (b, d)))
            )

        /** `f &&& g` at the Kleisli level: run both legs on the same input, no dup/swap stages. */
        def fanK[M[_], A, B, C](f: Kleisli[M, A, B], g: Kleisli[M, A, C])(using
            monadM: Monad[M]
        ): Kleisli[M, A, (B, C)] =
            Kleisli((a: A) => monadM.flatMap(f.run(a))(b => monadM.map(g.run(a))(c => (b, c))))

        /** `f ||| g` at the Kleisli level: dispatch on the Either, no merging `arr` stage. */
        def eitherK[M[_], A, B, C](
            f: Kleisli[M, A, C],
            g: Kleisli[M, B, C]
        ): Kleisli[M, Either[A, B], C] =
            Kleisli(_.fold(f.run, g.run))

        /** `f +++ g` at the Kleisli level. */
        def plusK[M[_], A, B, C, D](f: Kleisli[M, A, B], g: Kleisli[M, C, D])(using
            functorM: Functor[M]
        ): Kleisli[M, Either[A, C], Either[B, D]] =
            Kleisli(
              _.fold(
                a => functorM.map(f.run(a))(Left(_)),
                c => functorM.map(g.run(c))(Right(_))
              )
            )

        // The upstream equations: `Squelching l *** Emitting re rp = Emitting (id **** re) (l ****
        // rp)` etc. Squelching legs fold into whichever side of the Emitting pair keeps them
        // droppable.
        def split[M[_]: Monad, A, B, C, D](
            f: TracerA[M, A, B],
            g: TracerA[M, C, D]
        ): TracerA[M, (A, C), (B, D)] = (f, g) match {
            case (Squelching(l), Squelching(r))       => Squelching(parK(l, r))
            case (Squelching(l), Emitting(re, rp))    => Emitting(parK(idK, re), parK(l, rp))
            case (Emitting(le, lp), Squelching(r))    => Emitting(parK(le, idK), parK(lp, r))
            case (Emitting(le, lp), Emitting(re, rp)) => Emitting(parK(le, re), parK(lp, rp))
        }

        def merge[M[_]: Monad, A, B, C](
            f: TracerA[M, A, B],
            g: TracerA[M, A, C]
        ): TracerA[M, A, (B, C)] = (f, g) match {
            case (Squelching(l), Squelching(r))       => Squelching(fanK(l, r))
            case (Squelching(l), Emitting(re, rp))    => Emitting(fanK(idK[M, A], re), parK(l, rp))
            case (Emitting(le, lp), Squelching(r))    => Emitting(fanK(le, idK[M, A]), parK(lp, r))
            case (Emitting(le, lp), Emitting(re, rp)) => Emitting(fanK(le, re), parK(lp, rp))
        }

        def choice[M[_]: Monad, A, B, C](
            f: TracerA[M, A, C],
            g: TracerA[M, B, C]
        ): TracerA[M, Either[A, B], C] = (f, g) match {
            case (Squelching(l), Squelching(r))    => Squelching(eitherK(l, r))
            case (Squelching(l), Emitting(re, rp)) => Emitting(plusK(idK[M, A], re), eitherK(l, rp))
            case (Emitting(le, lp), Squelching(r)) => Emitting(plusK(le, idK[M, B]), eitherK(lp, r))
            case (Emitting(le, lp), Emitting(re, rp)) => Emitting(plusK(le, re), eitherK(lp, rp))
        }
    }

    given [M[_]: Monad]: Category[[X, Y] =>> TracerA[M, X, Y]] with {
        def id[A]: TracerA[M, A, A] = compute(x => x)

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            TracerAOps.compose(f, g)
    }

    given [M[_]: Monad]: Arrow[[X, Y] =>> TracerA[M, X, Y]] with {
        def lift[A, B](f: A => B): TracerA[M, A, B] = TracerAOps.lift(f)

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            TracerAOps.compose(f, g)

        def first[A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = TracerAOps.first(fa)

        override def split[A, B, C, D](
            f: TracerA[M, A, B],
            g: TracerA[M, C, D]
        ): TracerA[M, (A, C), (B, D)] = TracerAOps.split(f, g)

        override def merge[A, B, C](
            f: TracerA[M, A, B],
            g: TracerA[M, A, C]
        ): TracerA[M, A, (B, C)] = TracerAOps.merge(f, g)
    }

    given [M[_]: Monad]: ArrowChoice[[X, Y] =>> TracerA[M, X, Y]] with {
        def lift[A, B](f: A => B): TracerA[M, A, B] = TracerAOps.lift(f)

        def choose[A, B, C, D](
            f: TracerA[M, A, C]
        )(g: TracerA[M, B, D]): TracerA[M, Either[A, B], Either[C, D]] = (f, g) match {
            case (Squelching(l), Squelching(r)) => Squelching(l +++ r)
            case (Squelching(l), Emitting(re, rp)) =>
                Emitting(Category[[X, Y] =>> Kleisli[M, X, Y]].id +++ re, l +++ rp)
            case (Emitting(le, lp), Squelching(r)) =>
                Emitting(le +++ Category[[X, Y] =>> Kleisli[M, X, Y]].id, lp +++ r)
            case (Emitting(le, lp), Emitting(re, rp)) => Emitting(le +++ re, lp +++ rp)
        }

        def compose[A, B, C](f: TracerA[M, B, C], g: TracerA[M, A, B]): TracerA[M, A, C] =
            TracerAOps.compose(f, g)

        def first[A, B, C](fa: TracerA[M, A, B]): TracerA[M, (A, C), (B, C)] = TracerAOps.first(fa)

        // Without an explicit `choice` the class default fires:
        //   f ||| g = choose(f)(g) >>> lift(_.merge)
        // which inserts an extra lifted merge stage on every dispatch (mirrors the upstream
        // Haskell's hand-written (|||) equations).
        override def choice[A, B, C](
            f: TracerA[M, A, C],
            g: TracerA[M, B, C]
        ): TracerA[M, Either[A, B], C] = TracerAOps.choice(f, g)

        override def split[A, B, C, D](
            f: TracerA[M, A, B],
            g: TracerA[M, C, D]
        ): TracerA[M, (A, C), (B, D)] = TracerAOps.split(f, g)

        override def merge[A, B, C](
            f: TracerA[M, A, B],
            g: TracerA[M, A, C]
        ): TracerA[M, A, (B, C)] = TracerAOps.merge(f, g)
    }

    /** Changes the effect of every stage with `h`, keeping the tag. `h` must be a monad morphism,
      * preserving `pure` and `flatMap`, or the result depends on how the arrow was composed.
      */
    def nat[M[_], N[_], A, B](h: M ~> N)(t: TracerA[M, A, B]): TracerA[N, A, B] = t match {
        case Squelching(k)  => Squelching(k.mapK(h))
        case Emitting(k, l) => Emitting(k.mapK(h), l.mapK(h))
    }
