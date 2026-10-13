package hydrozoa.lib.logging.laws

import cats.Eq
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.MiniInt
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Generators.given
import hydrozoa.lib.logging.laws.Recorded.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite

/** Laws of [[TracerA]] that no cats suite states: the constructor tag, the observations' own
  * soundness, `first` against upstream's derived definition, and `nat`.
  */
object ArrowSemantics:
    import TracerA.given

    private val inputs: List[MiniInt] = ExhaustiveCheck[MiniInt].allValues

    private def eqFull[A: ExhaustiveCheck, B: Eq]: Eq[TA[A, B]] = Obs.eqTracerA(Obs.Full, true)

    private def tagOf(t: TracerA[?, ?, ?]): Boolean = isEmitting(t)

    /** T: `>>>`, `***`, `&&&`, `|||`, `+++`, `first` and `left` are Emitting iff a part is; `lift`
      * and `id` are Squelching.
      */
    val tagIsDisjunction: Prop =
        forAll(
          arbitrary[TA[MiniInt, Boolean]],
          arbitrary[TA[Boolean, Boolean]],
          arbitrary[TA[MiniInt, Boolean]],
          arbitrary[MiniInt => Boolean]
        ) { (f, g, h, k) =>
            tagOf(f >>> g) == (tagOf(f) || tagOf(g)) &&
            tagOf(f *** h) == (tagOf(f) || tagOf(h)) &&
            tagOf(f &&& h) == (tagOf(f) || tagOf(h)) &&
            tagOf(f ||| h) == (tagOf(f) || tagOf(h)) &&
            tagOf(f +++ g) == (tagOf(f) || tagOf(g)) &&
            tagOf(f.first[Boolean]) == tagOf(f) &&
            tagOf(f.left[Boolean]) == tagOf(f) &&
            !tagOf(TracerA.compute[W, MiniInt, Boolean](k)) &&
            !tagOf(summon[cats.arrow.Category[TA]].id[MiniInt])
        }

    /** Composing anything after a Squelching arrow, or a Squelching arrow after anything, keeps the
      * other side's tag; an Emitting side makes the composite Emitting.
      */
    val squelchingSideKeepsTag: Prop =
        forAll(arbitrary[TA[MiniInt, Boolean]], arbitrary[Boolean => Boolean]) { (f, k) =>
            val s = TracerA.effect[W, Boolean, Boolean](b => effected(1, b).as(k(b)))
            tagOf(f >>> s) == tagOf(f) &&
            tagOf(TracerA.compute[W, Boolean, MiniInt](_ => MiniInt.zero) >>> f) == tagOf(f)
        }

    /** Generator soundness: an Emitting arrow's non-emitting part records no Emit. */
    val nonEmitterNeverEmits: Prop = forAll { (t: TA[MiniInt, Boolean]) =>
        t match
            case TracerA.Emitting(e, _) =>
                inputs.forall { a =>
                    val head = log(e.run(a))
                    val all = log(flatten(t).run(a))
                    all.startsWith(head) && all.drop(head.size).forall(!_.isEmit)
                }
            case TracerA.Squelching(k) => inputs.forall(a => log(k.run(a)).forall(!_.isEmit))
    }

    /** Observation soundness: [[Recorded.compiled]] is the library's `compile` after discarding the
      * output, and equals it at `B = Unit`.
      */
    val compiledIsLibraryCompile: Prop =
        forAll(arbitrary[TA[MiniInt, Boolean]], arbitrary[TA[MiniInt, Unit]]) { (t, u) =>
            val discarded = t >>> TracerA.compute[W, Boolean, Unit](_ => ())
            inputs.forall { a =>
                log(compiled(t)(using monadW)(a)) == log(discarded.compile(using monadW)(a)) &&
                log(compiled(u)(using monadW)(a)) == log(u.compile(using monadW)(a)) &&
                log(u.runTracerA(using monadW).run(a)) == log(u.compile(using monadW)(a))
            }
        }

    /** S9: `first(f)` is `split(f, id)`, upstream's derived `first f = f *** id`, under Full with
      * the tag.
      */
    val firstIsSplitWithId: Prop = forAll { (f: TA[MiniInt, Boolean]) =>
        eqFull[(MiniInt, Boolean), (Boolean, Boolean)].eqv(
          f.first[Boolean],
          f *** summon[cats.arrow.Category[TA]].id[Boolean]
        )
    }

    /** `nat` by a monad morphism commutes with `>>>`, `&&&` and `|||` under Full, and keeps the
      * tag.
      */
    private def flatS[A, B](t: TracerA[S, A, B], a: A): (List[Ev], B) = runS(flatten(t).run(a))

    private def natA[A, B](t: TA[A, B]): TracerA[S, A, B] = TracerA.nat(toState)(t)

    val natCommutesWithComposition: Prop =
        forAll(
          arbitrary[TA[MiniInt, Boolean]],
          arbitrary[TA[Boolean, Boolean]],
          arbitrary[TA[MiniInt, Boolean]]
        ) { (f, g, h) =>
            val either: List[Either[MiniInt, MiniInt]] =
                inputs.flatMap(a => List(Left(a), Right(a)))
            tagOf(natA(f)) == tagOf(f) &&
            inputs.forall(a => flatS(natA(f >>> g), a) == flatS(natA(f) >>> natA(g), a)) &&
            inputs.forall(a => flatS(natA(f &&& h), a) == flatS(natA(f) &&& natA(h), a)) &&
            either.forall(e => flatS(natA(f ||| h), e) == flatS(natA(f) ||| natA(h), e))
        }

    /** The proposed `contramap` by `Kleisli.local` equals today's on the arrow itself, under Full
      * and Emits, with the tag.
      */
    val contramapLocalAgreesOnArrow: Prop =
        forAll(arbitrary[CT[MiniInt]], arbitrary[Boolean => MiniInt]) { (t, f) =>
            val local = Candidates.contramapLocal(t)(f).use
            val today = t.contramap(f).use
            Obs.eqTracerA[Boolean, Unit](Obs.Full, true).eqv(local, today) &&
            Obs.eqTracerA[Boolean, Unit](Obs.Emits, true).eqv(local, today)
        }

class TracerASemanticsSuite extends AnyFunSuite:
    val laws: List[(String, Prop)] = List(
      "T tag of a composite is the disjunction of its parts" -> ArrowSemantics.tagIsDisjunction,
      "T a Squelching side keeps the other side's tag" -> ArrowSemantics.squelchingSideKeepsTag,
      "an Emitting arrow's non-emitting part never emits" -> ArrowSemantics.nonEmitterNeverEmits,
      "the Compiled observation is the library's compile" ->
          ArrowSemantics.compiledIsLibraryCompile,
      "S9 first(f) is split(f, id)" -> ArrowSemantics.firstIsSplitWithId,
      "nat commutes with >>>, &&&, |||" -> ArrowSemantics.natCommutesWithComposition,
      "candidate contramapLocal equals contramap on the arrow" ->
          ArrowSemantics.contramapLocalAgreesOnArrow
    )

    laws.foreach { (name, prop) =>
        test(name) {
            val failure = PropCheck.failure(prop)
            assert(failure.isEmpty, failure.getOrElse(""))
        }
    }
