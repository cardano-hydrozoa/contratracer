package hydrozoa.lib.logging.laws

import cats.Eq
import cats.data.Kleisli
import cats.syntax.all.*
import cats.arrow.*
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Generators.given
import hydrozoa.lib.logging.laws.Recorded.*
import org.scalacheck.Test
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

type TA[X, Y] = TracerA[W, X, Y]

/** The cats rule sets for [[TracerA]] under a chosen observation. Built outside the suite class: a
  * local `given` in a method of an `AnyFunSuite` crashes the 3.3.7 compiler while it inlines
  * `test`.
  */
object ArrowRules:
    def category(obs: Obs, tag: Boolean, instance: Category[TA]): Laws#RuleSet =
        given [A: ExhaustiveCheck, B: Eq]: Eq[TA[A, B]] = Obs.eqTracerA(obs, tag)
        CategoryTests[TA](using instance).category[MiniInt, Boolean, MiniInt, Boolean]

    def arrow(obs: Obs, tag: Boolean, instance: Arrow[TA]): Laws#RuleSet =
        given [A: ExhaustiveCheck, B: Eq]: Eq[TA[A, B]] = Obs.eqTracerA(obs, tag)
        ArrowTests[TA](using instance).arrow[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean]

    def arrowChoice(obs: Obs, tag: Boolean, instance: ArrowChoice[TA]): Laws#RuleSet =
        given [A: ExhaustiveCheck, B: Eq]: Eq[TA[A, B]] = Obs.eqTracerA(obs, tag)
        ArrowChoiceTests[TA](using instance)
            .arrowChoice[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean]

    val surveyParams: Test.Parameters =
        Test.Parameters.default.withMinSuccessfulTests(300).withInitialSeed(Seed(20261013L))

    /** The names of the laws in `rules` that fail with [[surveyParams]]. */
    def failing(rules: Laws#RuleSet): List[String] =
        rules.all.properties.toList.collect {
            case (name, prop) if !Test.check(surveyParams, prop).passed => name
        }

/** cats' Category, Arrow and ArrowChoice laws for [[TracerA]], checked against the Category and
  * ArrowChoice givens by name (the Arrow given is checked in [[ArrowGivensLawsSuite]]).
  *
  * Under [[Obs.Emits]] every law holds, constructor tag included; the Category laws hold under
  * every observation. [[TracerAArrowDeparturesSuite]] pins the one law that fails under
  * [[Obs.Full]] and [[Obs.Compiled]].
  *
  * Discipline suites here hold only `checkAll`: on Scala 3.3.7 with scalatest 3.2.19, a class
  * mixing in `FunSuiteDiscipline` with more than one direct `test(...)` call fails to compile with
  * "[Internal error] unexpected tree" while inlining `test`.
  */
class TracerAArrowLawsSuite extends AnyFunSuite with FunSuiteDiscipline with Checkers:
    private val categoryGiven = TracerA.given_Category_TracerA[W]
    private val arrowChoiceGiven = TracerA.given_ArrowChoice_TracerA[W]

    checkAll("Category given, Emits+tag", ArrowRules.category(Obs.Emits, true, categoryGiven))
    checkAll("Category given, Full+tag", ArrowRules.category(Obs.Full, true, categoryGiven))
    checkAll(
      "Category given, Compiled+tag",
      ArrowRules.category(Obs.Compiled, true, categoryGiven)
    )
    checkAll(
      "ArrowChoice given, Emits+tag",
      ArrowRules.arrowChoice(Obs.Emits, true, arrowChoiceGiven)
    )

/** The one cats law [[TracerA]] fails, and why that is by design.
  *
  * `split consistent with andThen` (`split(f, g) == first(f) >>> second(g)`) fails under
  * [[Obs.Full]] and [[Obs.Compiled]]. `split` keeps each leg's non-emitting part in the
  * non-emitting position, where it stays droppable; `first(f) >>> second(g)` threads `f`'s output
  * through `g`'s emitting part, so `f`'s non-emitting part becomes part of the emission. The
  * upstream `(***)` equations (Control/Tracer/Arrow.hs, lines 78-81) make the same choice. Under
  * [[Obs.Emits]] the law holds: the two sides differ only in which non-emitting effects run and
  * when.
  */
class TracerAArrowDeparturesSuite extends AnyFunSuite:
    val arrowChoiceGiven: ArrowChoice[TA] = TracerA.given_ArrowChoice_TracerA[W]
    val departure: List[String] = List("arrowChoice.split consistent with andThen")
    val pairIn: (Boolean, Boolean) = (true, false)

    def failingUnder(obs: Obs): List[String] =
        ArrowRules.failing(ArrowRules.arrowChoice(obs, true, arrowChoiceGiven))

    def runFull(t: TA[(Boolean, Boolean), ?]): List[Ev] = log(flatten(t).run(pairIn))

    def runCompiled(t: TA[(Boolean, Boolean), ?]): List[Ev] =
        log(compiled(t)(using monadW)(pairIn))

    def emitThenEffect(e: Int, p: Int): TA[Boolean, Boolean] =
        TracerA.Emitting[W, Boolean, Boolean, Boolean](
          Kleisli(a => emitted(e, a).as(a)),
          Kleisli(a => effected(p, a).as(a))
        )

    test("under Full+tag, only split-consistent-with-andThen fails (fixed seed)") {
        assert(failingUnder(Obs.Full) == departure)
    }

    test("under Compiled+tag, only split-consistent-with-andThen fails (fixed seed)") {
        assert(failingUnder(Obs.Compiled) == departure)
    }

    test("split vs first >>> second, Full: non-emitting parts run after both emitters") {
        val f = emitThenEffect(1, 2)
        val g = emitThenEffect(3, 4)
        val split = arrowChoiceGiven.split(f, g)
        val sequenced = arrowChoiceGiven.andThen(
          arrowChoiceGiven.first[Boolean, Boolean, Boolean](f),
          arrowChoiceGiven.second[Boolean, Boolean, Boolean](g)
        )
        val e1 = Ev.Emit(1, "true")
        val p2 = Ev.Eff(2, "true")
        val e3 = Ev.Emit(3, "false")
        val p4 = Ev.Eff(4, "false")
        assert(runFull(split) == List(e1, e3, p2, p4) && runFull(sequenced) == List(e1, p2, e3, p4))
    }

    test("split vs first >>> second, Compiled: split drops a squelching leg's effect") {
        val f = TracerA.effect[W, Boolean, Boolean](a => effected(5, a).as(a))
        val g = TracerA.emit[W, Boolean](a => emitted(6, a))
        val split = arrowChoiceGiven.split(f, g)
        val sequenced = arrowChoiceGiven.andThen(
          arrowChoiceGiven.first[Boolean, Boolean, Boolean](f),
          arrowChoiceGiven.second[Boolean, Unit, Boolean](g)
        )
        assert(
          runCompiled(split) == List(Ev.Emit(6, "false")) &&
              runCompiled(sequenced) == List(Ev.Eff(5, "true"), Ev.Emit(6, "false"))
        )
    }
