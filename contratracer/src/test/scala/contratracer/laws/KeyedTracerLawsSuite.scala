package contratracer.laws

import cats.*
import cats.data.{Chain, Writer}
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import contratracer.*
import contratracer.laws.Recorded.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

type KT[X] = KeyedTracer[W, Boolean, X]

/** Generators of keyed tracers over [[W]], keyed by `Boolean`: leaves over the generated
  * `ContraTracer`s of [[Generators]], nodes through `|+|` and `contramap`.
  */
object KeyedGenerators:

    def keyed[A: Arbitrary: Cogen](depth: Int): Gen[KT[A]] =
        val leaves = Gen.oneOf(
          for
              enabled <- arbitrary[Boolean => Boolean]
              tracer <- Generators.tracer[(Boolean, A)](1)
          yield KeyedTracer(enabled, tracer),
          Gen.const(KeyedTracer.nullTracer[W, Boolean, A]),
          Generators.tracer[A](1).map(KeyedTracer.always[W, Boolean, A](_))
        )
        if depth <= 0 then leaves
        else
            Gen.frequency(
              3 -> leaves,
              2 -> (for
                  x <- keyed[A](depth - 1)
                  y <- keyed[A](depth - 1)
              yield x |+| y),
              1 -> (for
                  x <- keyed[A](depth - 1)
                  f <- arbitrary[A => A]
              yield x.contramap(f))
            )

    given arbKeyed[A: Arbitrary: Cogen]: Arbitrary[KT[A]] =
        Arbitrary(Gen.choose(0, 2).flatMap(keyed[A]))

/** The cats rule sets for [[KeyedTracer]], observed through `traceAt` on every key and input: the
  * same `enabled` answer, the same recorded events, and the same tag on the filtered tracer.
  */
object KeyedRules:
    import KeyedGenerators.given

    given [A](using ex: ExhaustiveCheck[A]): Eq[KT[A]] = Eq.instance { (x, y) =>
        isEmitting(x.filtered.runTracer) == isEmitting(y.filtered.runTracer) &&
        List(false, true).forall(k =>
            x.enabled(k) == y.enabled(k) &&
                ex.allValues.forall(a => log(x.traceAt(k)(a)) == log(y.traceAt(k)(a)))
        )
    }

    def contravariant: Laws#RuleSet =
        ContravariantTests[KT].contravariant[MiniInt, Boolean, Boolean]

    def monoid: Laws#RuleSet = MonoidTests[KT[MiniInt]].monoid

/** A keyed tracer whose every leaf records the keys it receives, with each leaf's own `enabled`. */
object Soundness:

    /** One leaf's record: the key it received, and what its own `enabled` says to that key. */
    final case class Seen(leaf: Int, key: Int, enabledThere: Boolean)

    type Log = Chain[Seen]
    type L[A] = Writer[Log, A]

    /** Levels 0 to 4, the higher the more severe. */
    val levels: Gen[Int] = Gen.choose(0, 4)

    private def leaf(id: Int, enabled: Int => Boolean): KeyedTracer[L, Int, Int] =
        KeyedTracer(
          enabled,
          ContraTracer.emit[L, (Int, Int)]((k, _) =>
              Writer.tell(Chain.one(Seen(id, k, enabled(k))))
          )
        )

    /** A threshold logger: a level is enabled when it is at least `t`. */
    def threshold(t: Int): Int => Boolean = _ >= t

    /** A tree of leaves and `|+|`, `contramap` and `always` nodes; each leaf's `enabled` comes from
      * `enabledGen`.
      */
    def tree(enabledGen: Gen[Int => Boolean], depth: Int): Gen[KeyedTracer[L, Int, Int]] =
        val leaves = for
            id <- Gen.choose(0, 999)
            enabled <- enabledGen
        yield leaf(id, enabled)
        if depth <= 0 then leaves
        else
            Gen.frequency(
              2 -> leaves,
              2 -> (for
                  x <- tree(enabledGen, depth - 1)
                  y <- tree(enabledGen, depth - 1)
              yield x |+| y),
              1 -> (for
                  x <- tree(enabledGen, depth - 1)
                  f <- arbitrary[Int => Int]
              yield x.contramap(f))
            )

    /** The event's own level, from a field: `byValue` with `bound` as its gate. */
    def key(bound: Int, keyOf: Int => Int): TraceKey[Int, Int] = TraceKey.byValue(bound)(keyOf)

/** cats' Contravariant and Monoid laws for [[KeyedTracer]], and its soundness: a sink receives an
  * event only under a key its own `enabled` accepted.
  */
class KeyedTracerLawsSuite
    extends AnyFunSuite
    with FunSuiteDiscipline
    with Checkers
    with LawEvaluations:
    import Soundness.*

    checkAll("KeyedTracer, traceAt", KeyedRules.contravariant)
    checkAll("KeyedTracer, traceAt", KeyedRules.monoid)

    test("soundness: every leaf receives only keys its own enabled accepts") {
        val anyEnabled: Gen[Int => Boolean] = arbitrary[Int => Boolean]
        check(
          forAll(Gen.choose(0, 3).flatMap(tree(anyEnabled, _)), levels, arbitrary[Int]) {
              (t, k, a) =>
                  val seen = t.traceAt(k)(a).written ++
                      t.traceWith(a)(using key(k, _ => k)).written ++
                      t.traceWith(a)(using key(k, e => Math.floorMod(e, 5))).written
                  seen.forall(_.enabledThere)
          }
        )
    }

    test("completeness: under threshold loggers and the most severe bound, nothing enabled drops") {
        val thresholds: Gen[Int => Boolean] = Gen.choose(0, 5).map(threshold)
        check(
          forAll(Gen.choose(0, 3).flatMap(tree(thresholds, _)), arbitrary[Int]) { (t, a) =>
              val level = Math.floorMod(a, 5)
              val viaKey = t.traceWith(a)(using key(4, e => Math.floorMod(e, 5))).written
              val viaCall = t.traceAt(level)(a).written
              viaKey == viaCall
          }
        )
    }
