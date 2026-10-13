package contratracer

import cats.Id
import cats.arrow.FunctionK
import contratracer.ContraTracer.nullTracer
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** `traceWith` takes its argument by name: a squelching tracer must never evaluate it, and an
  * emitting tracer must evaluate it exactly once, whatever combinators sit in between.
  */
class ByNameTraceWithSpec extends AnyFunSuite {

    private def boom: Int = throw new AssertionError("a squelched tracer forced its argument")

    private val squelched: List[(String, ContraTracer[Id, Int])] = List(
      "nullTracer" -> nullTracer[Id, Int],
      "|+|" -> (nullTracer[Id, Int] |+| nullTracer[Id, Int]),
      "contramap" -> nullTracer[Id, Int].contramap[Int](_ + 1),
      "contramapM" -> nullTracer[Id, Int].contramapM[Int](i => i + 1),
      "squelchUnless" -> nullTracer[Id, Int].squelchUnless(_ => true),
      "traceMaybe" -> nullTracer[Id, Int].traceMaybe[Int](Some(_)),
      "mapK" -> nullTracer[Id, Int].mapK(FunctionK.id[Id])
    )

    squelched.foreach { (name, tracer) =>
        test(s"squelched through $name: the argument is never evaluated") {
            tracer.traceWith(boom)
        }
    }

    private def emitting(
        sink: ListBuffer[Int]
    ): List[(String, ContraTracer[Id, Int], List[Int])] = {
        val leaf = ContraTracer.emit[Id, Int](i => sink += i: Unit)
        List(
          ("emit", leaf, List(7)),
          ("|+| with itself", leaf |+| leaf, List(7, 7)),
          ("|+| with nullTracer", leaf |+| nullTracer[Id, Int], List(7)),
          ("contramap", leaf.contramap[Int](_ + 1), List(8)),
          ("squelchUnless (passes)", leaf.squelchUnless(_ > 0), List(7)),
          ("squelchUnless (filtered)", leaf.squelchUnless(_ < 0), Nil),
          ("traceMaybe", leaf.traceMaybe[Int](Some(_)), List(7)),
          ("mapK", leaf.mapK(FunctionK.id[Id]), List(7))
        )
    }

    emitting(ListBuffer.empty).map(_._1).foreach { name =>
        test(s"emitting through $name: the argument is evaluated exactly once") {
            val sink = ListBuffer.empty[Int]
            val (_, tracer, expected) = emitting(sink).find(_._1 == name).get
            var evaluations = 0
            tracer.traceWith { evaluations += 1; 7 }
            assert((evaluations, sink.toList) == (1, expected))
        }
    }
}
