package hydrozoa.lib.logging.laws

import cats.*
import cats.arrow.FunctionK
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.MiniInt
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Generators.given
import hydrozoa.lib.logging.laws.Recorded.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Prop, Test}
import org.scalatest.funsuite.AnyFunSuite

/** Runs a property and renders its failure, for suites that assert properties directly. */
object PropCheck:
    val params: Test.Parameters = Test.Parameters.default.withMinSuccessfulTests(300)

    def failure(p: Prop): Option[String] =
        val r = Test.check(params, p)
        Option.when(!r.passed)(Pretty.pretty(r, Pretty.Params(2)))

/** The library's own laws, which no cats suite states. Tracers run in [[W]] on inputs of type
  * `MiniInt`; `traceWith` is observed through the events it records.
  */
object Semantics:

    private val inputs: List[MiniInt] = ExhaustiveCheck[MiniInt].allValues
    private val eqCT: Eq[CT[MiniInt]] = Obs.eqTracer[MiniInt](tag = true)

    def ran(t: CT[MiniInt], a: MiniInt): List[Ev] = log(t.traceWith(a))

    private def tagOf(t: ContraTracer[?, ?]): Boolean = isEmitting(t.runTracer)

    /** S1: a squelching tracer records nothing on any input. */
    val squelchingRunsNothing: Prop = forAll { (t: CT[MiniInt]) =>
        tagOf(t) || inputs.forall(a => ran(t, a).isEmpty)
    }

    /** S2: every squelching tracer, nullTracer among them, is a two-sided `|+|` identity, including
      * its effects and the tag.
      */
    val squelchingIsIdentity: Prop = forAll { (s: CT[MiniInt], t: CT[MiniInt]) =>
        tagOf(s) || (eqCT.eqv(s |+| t, t) && eqCT.eqv(t |+| s, t))
    }

    /** S3: `x |+| y` runs `x`'s effects, then `y`'s; its tag is the disjunction. */
    val combineIsLeftThenRight: Prop = forAll { (x: CT[MiniInt], y: CT[MiniInt]) =>
        tagOf(x |+| y) == (tagOf(x) || tagOf(y)) &&
        inputs.forall(a => ran(x |+| y, a) == ran(x, a) ++ ran(y, a))
    }

    /** S4: `contramapM(f)` runs `f` on every trace iff the tracer is tagged Emitting, whether or
      * not that trace then emits; it keeps the tag.
      */
    val contramapMRunsIffEmitting: Prop =
        forAll(Generators.genTag, arbitrary[CT[MiniInt]], arbitrary[MiniInt => MiniInt]) {
            (tag, t, f) =>
                val c = t.contramapM((a: MiniInt) => effected(tag, a).as(f(a)))
                tagOf(c) == tagOf(t) && inputs.forall { a =>
                    ran(c, a) ==
                        (if tagOf(t) then Ev.Eff(tag, a.toString) :: ran(t, f(a)) else Nil)
                }
        }

    /** S5: `squelchUnless(p)` traces exactly the inputs satisfying `p`, and keeps the tag. */
    val squelchUnlessFilters: Prop = forAll { (t: CT[MiniInt], p: MiniInt => Boolean) =>
        val s = t.squelchUnless(p)
        tagOf(s) == tagOf(t) && inputs.forall(a => ran(s, a) == (if p(a) then ran(t, a) else Nil))
    }

    /** S6: `traceMaybe(f)` traces `f(a)` when defined, nothing otherwise; keeps the tag. */
    val traceMaybeTraces: Prop = forAll { (t: CT[MiniInt], f: MiniInt => Option[MiniInt]) =>
        val m = t.traceMaybe(f)
        tagOf(m) == tagOf(t) && inputs.forall(a => ran(m, a) == f(a).fold(Nil)(ran(t, _)))
    }

    /** S6m: `traceMaybeM` runs its classifier iff the tracer is tagged Emitting. */
    val traceMaybeMTraces: Prop =
        forAll(Generators.genTag, arbitrary[CT[MiniInt]], arbitrary[MiniInt => Option[MiniInt]]) {
            (tag, t, f) =>
                val m = t.traceMaybeM((a: MiniInt) => effected(tag, a).as(f(a)))
                tagOf(m) == tagOf(t) && inputs.forall { a =>
                    ran(m, a) ==
                        (if tagOf(t) then Ev.Eff(tag, a.toString) :: f(a).fold(Nil)(ran(t, _))
                         else Nil)
                }
        }

    /** S7: `natTracer` by a monad morphism maps `traceWith` through it, keeps the tag, and commutes
      * with `|+|`, `contramap` and `contramapM`; by the identity it changes nothing.
      */
    private val eqS: Eq[ContraTracer[S, MiniInt]] = Obs.eqTracerS[MiniInt]

    private def nat(t: CT[MiniInt]): ContraTracer[S, MiniInt] = t.natTracer(toState)

    val natMapsTraceWith: Prop = forAll { (t: CT[MiniInt]) =>
        tagOf(nat(t)) == tagOf(t) && inputs.forall(a =>
            runS(nat(t).traceWith(a)) == (ran(t, a), ())
        )
    }

    val natCommutes: Prop =
        forAll(arbitrary[CT[MiniInt]], arbitrary[CT[MiniInt]], arbitrary[MiniInt => MiniInt]) {
            (x, y, f) =>
                eqS.eqv(nat(x |+| y), nat(x) |+| nat(y)) &&
                eqS.eqv(nat(x.contramap(f)), nat(x).contramap(f)) &&
                eqS.eqv(
                  nat(x.contramapM(a => effected(7, a).as(f(a)))),
                  nat(x).contramapM(a => toState(effected(7, a).as(f(a))))
                )
        }

    val natIdentity: Prop = forAll { (t: CT[MiniInt]) =>
        eqCT.eqv(t.natTracer(FunctionK.id[W]), t)
    }

    /** S8: `traceTraversable` traces each element in order; `traceAll(f)` is
      * `traceTraversable.contramap(f)`; both keep the tag.
      */
    val traceTraversableTraverses: Prop =
        forAll(arbitrary[CT[MiniInt]], Gen.listOf(arbitrary[MiniInt])) { (t, as) =>
            val tt = t.traceTraversable[List]
            tagOf(tt) == tagOf(t) && log(tt.traceWith(as)) == as.flatMap(ran(t, _))
        }

    val traceAllIsContramap: Prop =
        forAll(arbitrary[CT[MiniInt]], arbitrary[MiniInt => List[MiniInt]]) { (t, f) =>
            val all = t.traceAll[List, MiniInt](f)
            tagOf(all) == tagOf(t) &&
            inputs.forall(a => log(all.traceWith(a)) == f(a).flatMap(ran(t, _)))
        }

    /** The proposed `contramap` by `Kleisli.local` equals today's under traceWith and the tag. */
    val contramapLocalAgrees: Prop = forAll { (t: CT[MiniInt], f: MiniInt => MiniInt) =>
        eqCT.eqv(Candidates.contramapLocal(t)(f), t.contramap(f))
    }

    /** Candidate ContravariantMonoidal agrees with the library's Monoid: `x |+| y` is the product
      * contramapped by the diagonal.
      */
    val productIsCombine: Prop = forAll { (x: CT[MiniInt], y: CT[MiniInt]) =>
        val cm = Candidates.contravariantMonoidal[W]
        eqCT.eqv(cm.contramap(cm.product(x, y))((a: MiniInt) => (a, a)), x |+| y)
    }

    /** Candidate `decide` traces a `Left` with the left tracer, a `Right` with the right. */
    val decideDispatches: Prop = forAll { (x: CT[MiniInt], y: CT[MiniInt]) =>
        val d = Candidates.decide(x, y)
        tagOf(d) == (tagOf(x) || tagOf(y)) && inputs.forall { a =>
            log(d.traceWith(Left(a))) == ran(x, a) && log(d.traceWith(Right(a))) == ran(y, a)
        }
    }

/** The library's semantic laws (S1-S8) and the candidates' consistency with today's API. */
class ContraTracerSemanticsSuite extends AnyFunSuite:
    val laws: List[(String, Prop)] = List(
      "S1 a squelching tracer records nothing" -> Semantics.squelchingRunsNothing,
      "S2 every squelching tracer is a |+| identity, effects included" ->
          Semantics.squelchingIsIdentity,
      "S3 |+| runs left then right; tag is the disjunction" -> Semantics.combineIsLeftThenRight,
      "S4 contramapM's effect runs iff the tracer is tagged Emitting" ->
          Semantics.contramapMRunsIffEmitting,
      "S5 squelchUnless(p) traces exactly the inputs satisfying p" ->
          Semantics.squelchUnlessFilters,
      "S6 traceMaybe(f) traces f(a) when defined" -> Semantics.traceMaybeTraces,
      "S6m traceMaybeM's classifier runs iff the tracer is tagged Emitting" ->
          Semantics.traceMaybeMTraces,
      "S7 natTracer maps traceWith through the morphism" -> Semantics.natMapsTraceWith,
      "S7 natTracer commutes with |+|, contramap, contramapM" -> Semantics.natCommutes,
      "S7 natTracer by the identity changes nothing" -> Semantics.natIdentity,
      "S8 traceTraversable traces each element in order" -> Semantics.traceTraversableTraverses,
      "S8 traceAll(f) is traceTraversable contramapped by f" -> Semantics.traceAllIsContramap,
      "candidate contramapLocal equals contramap" -> Semantics.contramapLocalAgrees,
      "candidate ContravariantMonoidal product agrees with |+|" -> Semantics.productIsCombine,
      "candidate decide dispatches Left and Right" -> Semantics.decideDispatches
    )

    laws.foreach { (name, prop) =>
        test(name) {
            val failure = PropCheck.failure(prop)
            assert(failure.isEmpty, failure.getOrElse(""))
        }
    }
