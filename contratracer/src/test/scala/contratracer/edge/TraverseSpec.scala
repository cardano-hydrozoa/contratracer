package contratracer.edge

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import contratracer.ContraTracer
import contratracer.ContraTracer.nullTracer
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Try

/** `traceTraversable` and `traceAll` over large and infinite structures.
  *
  * A squelching tracer never touches the structure. An emitting one traverses with cats'
  * `traverseVoid` when the returned `M[Unit]` runs. In a suspending monad such as IO the call
  * applies the sink function to no element, and the run applies it to each element just before that
  * element's effect, in order, for `List`, `Vector` and `LazyList` alike.
  */
class TraverseSpec extends AnyFunSuite {

    private val large = 1000000

    private final class Enough extends RuntimeException("enough")

    test("squelched: an infinite LazyList is never traversed, and traceAll never builds one") {
        var built = 0
        val traced = nullTracer[Id, Int].traceTraversable[LazyList].traceWith(LazyList.from(0))
        val viaTraceAll = nullTracer[Id, Int].traceAll[LazyList, Int] { i =>
            built += 1; LazyList.from(i)
        }
        viaTraceAll.traceWith(0)
        assert((traced, built) == ((), 0))
    }

    test(s"Id: a List of $large is traced in order without overflowing") {
        var (count, last, ordered) = (0, -1, true)
        val t = ContraTracer.emit[Id, Int] { i =>
            ordered &&= i == last + 1; last = i; count += 1
        }
        t.traceTraversable[List].traceWith(List.range(0, large))
        assert((count, ordered) == (large, true))
    }

    List("List", "Vector", "LazyList").foreach { name =>
        test(s"IO: a $name of $large has its sink function applied per element as the IO runs") {
            var (applied, ran, interleaved) = (0, 0, true)
            val t = ContraTracer.emit[IO, Int] { _ =>
                interleaved &&= applied == ran; applied += 1; IO.delay(ran += 1)
            }
            val traced = name match {
                case "List"   => t.traceTraversable[List].traceWith(List.range(0, large))
                case "Vector" => t.traceTraversable[Vector].traceWith(Vector.range(0, large))
                case _        => t.traceTraversable[LazyList].traceWith(LazyList.range(0, large))
            }
            val atCall = (applied, ran)
            traced.unsafeRunSync()
            assert((atCall, (applied, ran), interleaved) == ((0, 0), (large, large), true))
        }
    }

    test("Id: an infinite LazyList is emitted element by element until the sink stops it") {
        var count = 0
        val t = ContraTracer.emit[Id, Int] { _ =>
            count += 1; if count == 10000 then throw Enough()
        }
        val outcome = Try(t.traceTraversable[LazyList].traceWith(LazyList.from(0)))
        assert((outcome.failed.toOption.map(_.getMessage), count) == (Some("enough"), 10000))
    }

    test("IO: an infinite LazyList traces lazily, and emits until the sink stops it") {
        var count = 0
        val t = ContraTracer.emit[IO, Int] { _ =>
            IO.delay(count += 1).flatMap(_ => IO.raiseWhen(count == 10000)(Enough()))
        }
        val traced = t.traceTraversable[LazyList].traceWith(LazyList.from(0))
        val outcome = traced.attempt.unsafeRunSync()
        assert((outcome.left.toOption.map(_.getMessage), count) == (Some("enough"), 10000))
    }
}
