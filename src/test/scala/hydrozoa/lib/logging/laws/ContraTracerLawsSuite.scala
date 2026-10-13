package hydrozoa.lib.logging.laws

import cats.*
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Generators.given
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

type CT[X] = ContraTracer[W, X]

/** Instances and variants the library does not have, checked here as candidates. */
object Candidates:
    import TracerA.given

    /** `product` traces each half of a pair with its own tracer, left then right; `unit` is
      * `nullTracer`. This is Haskell's `Divisible` (`divide`, `conquer`) in cats' encoding.
      */
    def contravariantMonoidal[M[_]: Monad]: ContravariantMonoidal[[X] =>> ContraTracer[M, X]] =
        new ContravariantMonoidal[[X] =>> ContraTracer[M, X]]:
            def unit: ContraTracer[M, Unit] = ContraTracer.nullTracer[M, Unit]

            def product[A, B](
                fa: ContraTracer[M, A],
                fb: ContraTracer[M, B]
            ): ContraTracer[M, (A, B)] =
                ContraTracer(
                  (fa.use *** fb.use) >>> TracerA.compute[M, (Unit, Unit), Unit](_ => ())
                )

            def contramap[A, B](fa: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
                Contravariant[[X] =>> ContraTracer[M, X]].contramap(fa)(f)

    /** `contramap` that pre-composes `f` onto the first Kleisli instead of composing a lifted stage
      * in front of the tracer.
      */
    def contramapLocal[M[_]: Monad, A, B](t: ContraTracer[M, A])(f: B => A): ContraTracer[M, B] =
        t.runTracer match
            case TracerA.Emitting(e, p) => ContraTracer(TracerA.Emitting(e.local(f), p))
            case TracerA.Squelching(k)  => ContraTracer(TracerA.Squelching(k.local(f)))

    /** The Haskell `Decidable` operations in tracer form: `choose` dispatches an `Either` to one of
      * two tracers; `lose` (on `Nothing`) is `nullTracer`.
      */
    def decide[M[_]: Monad, A, B](
        fa: ContraTracer[M, A],
        fb: ContraTracer[M, B]
    ): ContraTracer[M, Either[A, B]] = ContraTracer(fa.use ||| fb.use)

/** The cats rule sets for [[ContraTracer]] under `traceWith` on every input, every recorded event
  * compared, constructor tag included.
  */
object ContraRules:
    given [A: ExhaustiveCheck]: Eq[CT[A]] = Obs.eqTracer[A](tag = true)

    def contravariant: Laws#RuleSet =
        ContravariantTests[CT].contravariant[MiniInt, Boolean, Boolean]

    def monoid: Laws#RuleSet = MonoidTests[CT[MiniInt]].monoid

    def contravariantMonoidal: Laws#RuleSet =
        given ContravariantMonoidal[CT] = Candidates.contravariantMonoidal[W]
        given Isomorphisms[CT] = Isomorphisms.invariant[CT]
        ContravariantMonoidalTests[CT].contravariantMonoidal[MiniInt, Boolean, Boolean]

/** cats' Contravariant and Monoid laws for [[ContraTracer]], and ContravariantMonoidal for the
  * candidate instance in [[Candidates]].
  */
class ContraTracerLawsSuite extends AnyFunSuite with FunSuiteDiscipline with Checkers:
    checkAll("ContraTracer, traceWith+tag", ContraRules.contravariant)
    checkAll("ContraTracer, traceWith+tag", ContraRules.monoid)
    checkAll("ContraTracer candidate, traceWith+tag", ContraRules.contravariantMonoidal)
