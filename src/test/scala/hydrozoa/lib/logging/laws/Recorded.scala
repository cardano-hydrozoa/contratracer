package hydrozoa.lib.logging.laws

import cats.*
import cats.data.*
import cats.laws.discipline.ExhaustiveCheck
import cats.syntax.all.*
import hydrozoa.lib.logging.*

/** One recorded effect of a test tracer. `Emit` is written only by an emitting effect (`emit`, or
  * the emitter half of a raw `Emitting`); `Eff` only by a non-emitting one (`effect`, or the
  * non-emitter half). The tag says which generated node ran; `in` is the input it saw.
  */
enum Ev:
    case Emit(tag: Int, in: String)
    case Eff(tag: Int, in: String)

    def isEmit: Boolean = this match
        case Emit(_, _) => true
        case Eff(_, _)  => false

/** The monad law suites run tracers in: a writer whose log is every effect that ran, in order. */
type W[A] = Writer[Chain[Ev], A]

/** The target of `natTracer` in the naturality laws. */
type S[A] = State[Vector[Ev], A]

object Recorded:
    val monadW: Monad[W] = summon[Monad[W]]

    def emitted(tag: Int, in: Any): W[Unit] = Writer.tell(Chain.one(Ev.Emit(tag, in.toString)))

    def effected(tag: Int, in: Any): W[Unit] = Writer.tell(Chain.one(Ev.Eff(tag, in.toString)))

    def log[A](w: W[A]): List[Ev] = w.written.toList

    /** A monad morphism `W ~> S`: appends the writer's log to the state. */
    val toState: W ~> S = new (W ~> S):
        def apply[A](w: W[A]): S[A] =
            val (l, a) = w.run
            State(s => (s ++ l.toList, a))

    /** A monad morphism `S ~> W` (from the empty state): tells the state the run produced. */
    val fromState: S ~> W = new (S ~> W):
        def apply[A](s: S[A]): W[A] =
            val (l, a) = s.run(Vector.empty).value
            Writer(Chain.fromSeq(l), a)

    def runS[A](s: S[A]): (List[Ev], A) =
        val (l, a) = s.run(Vector.empty).value
        (l.toList, a)

    /** Which constructor a tracer arrow carries: `true` for `Emitting`. */
    def isEmitting(t: TracerA[?, ?, ?]): Boolean = t match
        case TracerA.Emitting(_, _) => true
        case TracerA.Squelching(_)  => false

    /** The whole computation of `t`, emitting and non-emitting parts both: what any downstream
      * emitter consuming `t`'s output would run.
      */
    def flatten[M[_]: FlatMap, A, B](t: TracerA[M, A, B]): Kleisli[M, A, B] = t match
        case TracerA.Emitting(e, p) => e.andThen(p)
        case TracerA.Squelching(k)  => k

    /** What `traceWith` runs for `t` once its output is discarded (`t >>> squelch`): the emitting
      * part only.
      */
    def compiled[M[_], A, B](t: TracerA[M, A, B])(using M: Monad[M]): A => M[Unit] = t match
        case TracerA.Emitting(e, _) => a => M.void(e.run(a))
        case TracerA.Squelching(_)  => _ => M.unit

/** How a law suite observes a tracer arrow. */
enum Obs:
    /** Flattened; output and `Emit` events only (effects are benign, so unobserved). */
    case Emits

    /** Flattened; output and every event in order. */
    case Full

    /** As `traceWith` runs it: emitting part only; every event that ran. */
    case Compiled

object Obs:
    import Recorded.*

    def observe[A, B](obs: Obs, t: TracerA[W, A, B], a: A): (List[Ev], Option[B]) = obs match
        case Obs.Emits =>
            val (l, b) = flatten(t).run(a).run
            (l.toList.filter(_.isEmit), Some(b))
        case Obs.Full =>
            val (l, b) = flatten(t).run(a).run
            (l.toList, Some(b))
        case Obs.Compiled => (log(compiled(t)(using monadW)(a)), None)

    /** Equality of tracer arrows by observation on every input, optionally also requiring the same
      * constructor tag.
      */
    def eqTracerA[A, B](obs: Obs, tag: Boolean)(using
        ex: ExhaustiveCheck[A],
        eqB: Eq[B]
    ): Eq[TracerA[W, A, B]] = Eq.instance { (x, y) =>
        (!tag || isEmitting(x) == isEmitting(y)) &&
        ex.allValues.forall { a =>
            val (lx, bx) = observe(obs, x, a)
            val (ly, by) = observe(obs, y, a)
            lx == ly && (bx, by).tupled.forall(eqB.eqv(_, _))
        }
    }

    /** Equality of contravariant tracers by `traceWith` on every input: every event that ran. */
    def eqTracer[A](tag: Boolean)(using ex: ExhaustiveCheck[A]): Eq[ContraTracer[W, A]] =
        Eq.instance { (x, y) =>
            (!tag || isEmitting(x.runTracer) == isEmitting(y.runTracer)) &&
            ex.allValues.forall(a => log(x.traceWith(a)) == log(y.traceWith(a)))
        }

    def eqTracerS[A](using ex: ExhaustiveCheck[A]): Eq[ContraTracer[S, A]] =
        Eq.instance { (x, y) =>
            isEmitting(x.runTracer) == isEmitting(y.runTracer) &&
            ex.allValues.forall(a => runS(x.traceWith(a)) == runS(y.traceWith(a)))
        }
