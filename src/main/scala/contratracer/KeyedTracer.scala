package contratracer

import cats.{Contravariant, Monad, Monoid, ~>}

/** A tracer that asks whether it wants an event before the event exists.
  *
  * Each event travels with a key the caller can state without building it; a logging sink makes the
  * key a level. [[traceAt]] and [[traceWith]] ask `enabled(key)` first and evaluate the event only
  * on a yes, so a disabled event costs one call to `enabled` and allocates nothing. The key then
  * reaches the sink with the event, so a level is stated once: at the call site, or in a
  * [[TraceKey]] instance beside the event type. The library gives the key no meaning beyond
  * `enabled`.
  *
  * A `KeyedTracer` means the plain tracer of `(K, A)` that `tracer` is behind a filter on
  * `enabled`. [[contramap]], [[|+|]] and [[mapK]] keep that meaning, so the `Contravariant` and
  * `Monoid` laws are `ContraTracer`'s, applied to pairs.
  *
  * @param enabled
  *   asked before each event is built. It must be cheap, and it must say yes to every key whose
  *   events `tracer` would act on: a key it refuses is never traced.
  * @param tracer
  *   receives each enabled event, paired with its key
  * @tparam M
  *   the effect monad events are emitted into
  * @tparam K
  *   the key, known before the event is built, such as a level
  * @tparam A
  *   the event type this tracer consumes
  */
final class KeyedTracer[M[_], K, A](
    val enabled: K => Boolean,
    val tracer: ContraTracer[M, (K, A)]
)(using Monad[M]) {

    private val skipped: M[Unit] = Monad[M].unit

    /** Traces `a` under the key `k`. When `enabled(k)` says no, `a` is never evaluated and the
      * result is `unit`; otherwise this is `tracer.traceWith((k, a))`, with its timing.
      */
    def traceAt(k: K)(a: => A): M[Unit] =
        if enabled(k) then tracer.traceWith((k, a)) else skipped

    /** Traces `e` under the key its static type carries, found as a `TraceKey[K, E]`.
      *
      * `E` is the static type of the argument expression. A case class constructor or a case object
      * has its own type, but an enum case's `apply` returns the enum type, and
      * `if c then A(..) else B(..)` has the common parent's type; the instance must exist for that
      * type.
      *
      * `enabled(key.bound)` is asked before `e` is built, and a no ends the trace. For a key that
      * depends on the event's fields, `e` is then built and `enabled` asked again with the event's
      * own key.
      */
    def traceWith[E <: A](e: => E)(using key: TraceKey[K, E]): M[Unit] =
        if !enabled(key.bound) then skipped
        else if !key.varies then tracer.traceWith((key.bound, e))
        else {
            val built = e
            val k = key.keyOf(built)
            if enabled(k) then tracer.traceWith((k, built)) else skipped
        }

    /** Adapts the event with a pure function, run in front of this tracer. The key and `enabled`
      * are unchanged, so `f` never runs for a disabled key.
      */
    def contramap[B](f: B => A): KeyedTracer[M, K, B] =
        KeyedTracer(enabled, tracer.contramap[(K, B)]((k, b) => (k, f(b))))

    /** Traces each event into this tracer, then into `that`. A key is enabled when either tracer
      * enables it, and each tracer still receives only the keys its own `enabled` accepts. The
      * other contracts are `ContraTracer.|+|`'s; [[KeyedTracer.nullTracer]] is the identity.
      */
    def |+|(that: KeyedTracer[M, K, A]): KeyedTracer[M, K, A] =
        KeyedTracer(k => enabled(k) || that.enabled(k), filtered |+| that.filtered)

    /** Changes the effect with a natural transformation, as `ContraTracer.mapK` does. `enabled` is
      * unchanged.
      */
    def mapK[N[_]: Monad](h: M ~> N): KeyedTracer[N, K, A] =
        KeyedTracer(enabled, tracer.mapK(h))

    /** The plain tracer this one means: `tracer`, passed only the keys `enabled` accepts. It
      * squelches when `tracer` does.
      */
    def filtered: ContraTracer[M, (K, A)] = tracer.squelchUnless((k, _) => enabled(k))
}

object KeyedTracer {

    /** A tracer that enables every key and hands `tracer` the event alone, such as a test's
      * recording tracer combined with a keyed sink.
      */
    def always[M[_]: Monad, K, A](tracer: ContraTracer[M, A]): KeyedTracer[M, K, A] =
        KeyedTracer(_ => true, tracer.contramap[(K, A)](_._2))

    /** A tracer that enables no key and so builds no event. It is `Monoid.empty`, the identity of
      * `|+|`.
      */
    def nullTracer[M[_]: Monad, K, A]: KeyedTracer[M, K, A] =
        KeyedTracer(_ => false, ContraTracer.nullTracer)

    /** [[KeyedTracer.contramap]], for code generic over `Contravariant`. */
    given [M[_], K]: Contravariant[[X] =>> KeyedTracer[M, K, X]] with {
        override def contramap[A, B](t: KeyedTracer[M, K, A])(f: B => A): KeyedTracer[M, K, B] =
            t.contramap(f)
    }

    /** [[KeyedTracer.|+|]] with [[KeyedTracer.nullTracer]] as its identity. */
    given [M[_]: Monad, K, A]: Monoid[KeyedTracer[M, K, A]] with {
        override def empty: KeyedTracer[M, K, A] = nullTracer

        /** Starts the fold from the first tracer. `Monoid`'s default starts from `nullTracer`,
          * which adds a `|+|` to every trace.
          */
        override def combineAll(as: IterableOnce[KeyedTracer[M, K, A]]): KeyedTracer[M, K, A] =
            combineAllOption(as).getOrElse(empty)

        override def combineAllOption(
            as: IterableOnce[KeyedTracer[M, K, A]]
        ): Option[KeyedTracer[M, K, A]] =
            as.iterator.toVector.reduceRightOption(combine)

        override def combine(
            x: KeyedTracer[M, K, A],
            y: KeyedTracer[M, K, A]
        ): KeyedTracer[M, K, A] = x |+| y
    }
}

/** The key every event of type `E` is traced under, for [[KeyedTracer.traceWith]]. Put the instance
  * in `E`'s companion, where `traceWith` finds it without an import.
  *
  * `bound` is asked before the event is built. When the key depends on the event's fields, `bound`
  * must be the key whose being enabled follows from any key the event can take being enabled: for
  * levels, the most severe. A bound below that drops enabled events, and no type checks it.
  *
  * @tparam K
  *   the key
  * @tparam E
  *   the event type the key belongs to
  */
final class TraceKey[K, E] private (
    val bound: K,
    private[contratracer] val varies: Boolean,
    keyOfEvent: E => K
) {

    /** The key `e` is traced under: `bound` for a [[TraceKey.const]] key. */
    def keyOf(e: E): K = keyOfEvent(e)
}

object TraceKey {

    /** Every event of type `E` takes the key `k`. */
    def const[K, E](k: K): TraceKey[K, E] = new TraceKey(k, false, _ => k)

    /** The key depends on the event: `keyOf(e)` names it, and `bound` gates before `e` exists.
      *
      * `bound` must be enabled whenever any key `keyOf` can return is enabled; for levels, it is
      * the most severe one. With a lower bound, an event whose own key is enabled is dropped
      * whenever `bound` is not.
      */
    def byValue[K, E](bound: K)(keyOf: E => K): TraceKey[K, E] = new TraceKey(bound, true, keyOf)
}
