package contratracer.laws

import cats.data.Kleisli
import cats.syntax.all.*
import contratracer.*
import contratracer.laws.Recorded.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}

/** Generators of tracer arrows and contravariant tracers over [[W]].
  *
  * Every effect a generated tracer runs is recorded with a fresh tag, emitting effects as
  * [[Ev.Emit]] and non-emitting ones as [[Ev.Eff]], so an observation shows which nodes ran and in
  * what order. Leaves cover each constructor directly (including an `Emitting` whose non-emitting
  * part has an effect) and through the public combinators; inner nodes use `>>>`, `&&&`, `***`,
  * `|||`, `first` and `left`.
  */
object Generators:
    import TracerA.given

    private val maxDepth = 3

    val genTag: Gen[Int] = Gen.choose(0, 9999)

    def leaf[A: Cogen, B: Arbitrary]: Gen[TracerA[W, A, B]] = Gen.oneOf(
      arbitrary[A => B].map(f => TracerA.compute[W, A, B](f)),
      for
          t <- genTag
          f <- arbitrary[A => B]
      yield TracerA.effect[W, A, B](a => effected(t, a).as(f(a))),
      for
          t <- genTag
          u <- genTag
          f <- arbitrary[A => B]
      yield TracerA.Emitting[W, A, A, B](
        Kleisli(a => emitted(t, a).as(a)),
        Kleisli(a => effected(u, a).as(f(a)))
      ),
      for
          t <- genTag
          f <- arbitrary[A => B]
      yield (TracerA.emit[W, A](a => emitted(t, a)) &&& TracerA.compute[W, A, B](f)) >>>
          TracerA.compute[W, (Unit, B), B](_._2)
    )

    def tracerA[A: Arbitrary: Cogen, B: Arbitrary: Cogen](depth: Int): Gen[TracerA[W, A, B]] =
        if depth <= 0 then leaf[A, B]
        else
            val d = depth - 1
            Gen.frequency(
              4 -> leaf[A, B],
              1 -> (for
                  f <- tracerA[A, A](d)
                  g <- tracerA[A, B](d)
              yield f >>> g),
              1 -> (for
                  f <- tracerA[A, B](d)
                  g <- tracerA[B, B](d)
              yield f >>> g),
              1 -> (for
                  f <- tracerA[A, B](d)
                  g <- tracerA[A, B](d)
                  left <- arbitrary[Boolean]
              yield (f &&& g) >>> TracerA.compute[W, (B, B), B](p => if left then p._1 else p._2)),
              1 -> (for
                  f <- tracerA[A, B](d)
                  g <- tracerA[A, B](d)
                  left <- arbitrary[Boolean]
              yield TracerA.compute[W, A, (A, A)](a => (a, a)) >>> (f *** g) >>>
                  TracerA.compute[W, (B, B), B](p => if left then p._1 else p._2)),
              1 -> (for
                  f <- tracerA[A, B](d)
                  g <- tracerA[A, B](d)
                  p <- arbitrary[A => Boolean]
              yield TracerA.compute[W, A, Either[A, A]](a => if p(a) then Left(a) else Right(a))
                  >>> (f ||| g)),
              1 -> (for f <- tracerA[A, B](d)
              yield TracerA.compute[W, A, (A, A)](a => (a, a)) >>> f.first[A] >>>
                  TracerA.compute[W, (B, A), B](_._1)),
              1 -> (for
                  f <- tracerA[A, B](d)
                  g <- arbitrary[A => B]
                  p <- arbitrary[A => Boolean]
              yield TracerA.compute[W, A, Either[A, A]](a => if p(a) then Left(a) else Right(a))
                  >>> f.left[A] >>> TracerA.compute[W, Either[B, A], B](_.fold(identity, g)))
            )

    given arbTracerA[A: Arbitrary: Cogen, B: Arbitrary: Cogen]: Arbitrary[TracerA[W, A, B]] =
        Arbitrary(Gen.choose(0, maxDepth).flatMap(tracerA[A, B]))

    def tracer[A: Arbitrary: Cogen](depth: Int): Gen[ContraTracer[W, A]] =
        val leaves = Gen.oneOf(
          genTag.map(t => ContraTracer.emit[W, A](a => emitted(t, a))),
          Gen.const(ContraTracer.nullTracer[W, A]),
          tracerA[A, Unit](0).map(ContraTracer(_))
        )
        if depth <= 0 then leaves
        else
            val d = depth - 1
            Gen.frequency(
              3 -> leaves,
              1 -> tracerA[A, Unit](d).map(ContraTracer(_)),
              2 -> (for
                  x <- tracer[A](d)
                  y <- tracer[A](d)
              yield x |+| y),
              1 -> (for
                  x <- tracer[A](d)
                  p <- arbitrary[A => Boolean]
              yield x.squelchUnless(p)),
              1 -> (for
                  x <- tracer[A](d)
                  f <- arbitrary[A => A]
              yield x.contramap(f)),
              1 -> (for
                  x <- tracer[A](d)
                  t <- genTag
                  f <- arbitrary[A => A]
              yield x.contramapM(a => effected(t, a).as(f(a)))),
              1 -> (for
                  x <- tracer[A](d)
                  f <- arbitrary[A => Option[A]]
              yield x.traceMaybe(f))
            )

    given arbTracer[A: Arbitrary: Cogen]: Arbitrary[ContraTracer[W, A]] =
        Arbitrary(Gen.choose(0, maxDepth).flatMap(tracer[A]))
