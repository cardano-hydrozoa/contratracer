package contratracer.edge

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.{Id, ~>}
import contratracer.ContraTracer.nullTracer
import contratracer.{ContraTracer, TracerA}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** `mapK` maps the tracer's whole emitting arrow through the transformation, so it keeps the
  * Squelching tag and applies the transformation to the arrow's result once each time the traced
  * effect runs. It distributes over the other combinators only when the transformation is a monad
  * morphism. A transformation out of `Id` receives a value whose effects have already happened,
  * because the `Id` arrow runs as the transformation's argument.
  */
class NatTracerSpec extends AnyFunSuite {

    private final class Counting extends (IO ~> IO) {
        var applied = 0
        def apply[A](fa: IO[A]): IO[A] = { applied += 1; fa }
    }

    test("a squelching tracer stays squelching and never applies the transformation") {
        val h = Counting()
        val mapped = nullTracer[IO, Int].contramap[Int](_ + 1).mapK(h)
        val isSquelching = mapped.runTracer match {
            case TracerA.Squelching(_) => true
            case _                     => false
        }
        mapped.traceWith(1).unsafeRunSync()
        assert((isSquelching, h.applied) == (true, 0))
    }

    test("the transformation is applied once per run of the traced IO, whatever is inside") {
        val h = Counting()
        val leaf = ContraTracer.emit[IO, Int](_ => IO.unit)
        val mapped =
            (leaf |+| leaf |+| leaf.contramap[Int](_ + 1)).squelchUnless(_ > 0).mapK(h)
        val traced = mapped.traceWith(1)
        val atCall = h.applied
        traced.unsafeRunSync()
        traced.unsafeRunSync()
        assert((atCall, h.applied) == (0, 2))
    }

    test("a transformation that is not a monad morphism does not distribute over |+|") {
        val twice = new (IO ~> IO) { def apply[A](fa: IO[A]): IO[A] = fa *> fa }
        val seen = ListBuffer.empty[String]
        def leaf(name: String) = ContraTracer.emit[IO, Int](_ => IO.delay(seen += name: Unit))
        (leaf("a") |+| leaf("b")).mapK(twice).traceWith(1).unsafeRunSync()
        val outside = seen.toList
        seen.clear()
        (leaf("a").mapK(twice) |+| leaf("b").mapK(twice)).traceWith(1).unsafeRunSync()
        assert((outside, seen.toList) == (List("a", "b", "a", "b"), List("a", "a", "b", "b")))
    }

    private val idToIO = new (Id ~> IO) { def apply[A](a: Id[A]): IO[A] = IO.delay(a) }

    test("Id ~> IO: the Id sink runs on every run of the IO, with or without a stage in front") {
        val seen = ListBuffer.empty[Int]
        val mapped = ContraTracer.emit[Id, Int](i => seen += i: Unit).mapK(idToIO)
        val outcomes = List(mapped, mapped.contramap[Int](identity)).map { tracer =>
            seen.clear()
            val traced = tracer.traceWith(1)
            val atCall = seen.toList
            traced.unsafeRunSync()
            traced.unsafeRunSync()
            (atCall, seen.toList)
        }
        assert(outcomes == List.fill(2)((Nil, List(1, 1))))
    }

    test("IO ~> Id through unsafeRunSync: the IO sink runs at call time, on the caller") {
        val runNow = new (IO ~> Id) { def apply[A](fa: IO[A]): Id[A] = fa.unsafeRunSync() }
        val seen = ListBuffer.empty[Int]
        ContraTracer.emit[IO, Int](i => IO.delay(seen += i: Unit)).mapK(runNow).traceWith(1)
        assert(seen.toList == List(1))
    }
}
