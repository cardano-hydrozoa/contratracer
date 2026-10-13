package contratracer.edge

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import cats.{Id, ~>}
import contratracer.ContraTracer.nullTracer
import contratracer.{ContraTracer, TracerA}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** `natTracer` maps the tracer's whole emitting arrow through the transformation, so it keeps the
  * Squelching tag and applies the transformation once per trace, at call time, to the arrow's
  * result. It distributes over the other combinators only when the transformation is a monad
  * morphism, and a transformation out of `Id` sees effects that already happened.
  */
class NatTracerSpec extends AnyFunSuite {

    private final class Counting extends (IO ~> IO) {
        var applied = 0
        def apply[A](fa: IO[A]): IO[A] = { applied += 1; fa }
    }

    test("a squelching tracer stays squelching and never applies the transformation") {
        val h = Counting()
        val mapped = nullTracer[IO, Int].contramap[Int](_ + 1).natTracer(h)
        val isSquelching = mapped.runTracer match {
            case TracerA.Squelching(_) => true
            case _                     => false
        }
        mapped.traceWith(1).unsafeRunSync()
        assert((isSquelching, h.applied) == (true, 0))
    }

    test("the transformation is applied once per trace, at call time, whatever is inside") {
        val h = Counting()
        val leaf = ContraTracer.emit[IO, Int](_ => IO.unit)
        val mapped =
            (leaf |+| leaf |+| leaf.contramap[Int](_ + 1)).squelchUnless(_ > 0).natTracer(h)
        val traced = mapped.traceWith(1)
        val atCall = h.applied
        traced.unsafeRunSync()
        traced.unsafeRunSync()
        assert((atCall, h.applied) == (1, 1))
    }

    test("a transformation that is not a monad morphism does not distribute over |+|") {
        val twice = new (IO ~> IO) { def apply[A](fa: IO[A]): IO[A] = fa *> fa }
        val seen = ListBuffer.empty[String]
        def leaf(name: String) = ContraTracer.emit[IO, Int](_ => IO.delay(seen += name: Unit))
        (leaf("a") |+| leaf("b")).natTracer(twice).traceWith(1).unsafeRunSync()
        val outside = seen.toList
        seen.clear()
        (leaf("a").natTracer(twice) |+| leaf("b").natTracer(twice)).traceWith(1).unsafeRunSync()
        assert((outside, seen.toList) == (List("a", "b", "a", "b"), List("a", "a", "b", "b")))
    }

    private val idToIO = new (Id ~> IO) { def apply[A](a: Id[A]): IO[A] = IO.delay(a) }

    test("Id ~> IO: the Id sink runs at call time, and running the IO does not re-emit") {
        val seen = ListBuffer.empty[Int]
        val traced = ContraTracer.emit[Id, Int](i => seen += i: Unit).natTracer(idToIO).traceWith(1)
        val atCall = seen.toList
        traced.unsafeRunSync()
        traced.unsafeRunSync()
        assert((atCall, seen.toList) == (List(1), List(1)))
    }

    test("Id ~> IO then contramap in IO: the same Id sink runs on every run instead") {
        val seen = ListBuffer.empty[Int]
        val traced = ContraTracer
            .emit[Id, Int](i => seen += i: Unit)
            .natTracer(idToIO)
            .contramap[Int](identity)
            .traceWith(1)
        val atCall = seen.toList
        traced.unsafeRunSync()
        traced.unsafeRunSync()
        assert((atCall, seen.toList) == (Nil, List(1, 1)))
    }

    test("IO ~> Id through unsafeRunSync: the IO sink runs at call time, on the caller") {
        val runNow = new (IO ~> Id) { def apply[A](fa: IO[A]): Id[A] = fa.unsafeRunSync() }
        val seen = ListBuffer.empty[Int]
        ContraTracer.emit[IO, Int](i => IO.delay(seen += i: Unit)).natTracer(runNow).traceWith(1)
        assert(seen.toList == List(1))
    }
}
