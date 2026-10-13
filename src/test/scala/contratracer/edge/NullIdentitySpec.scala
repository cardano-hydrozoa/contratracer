package contratracer.edge

import cats.Id
import cats.syntax.all.*
import contratracer.{ContraTracer, TracerA}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** `nullTracer` is a free identity of `|+|`: combined with it, a tracer comes back as itself. Only
  * a genuine `nullTracer` is dropped; another squelching tracer keeps the stages it runs when the
  * other side emits.
  */
class NullIdentitySpec extends AnyFunSuite {

    private def sink(out: ListBuffer[String]): ContraTracer[Id, Int] =
        ContraTracer.emit[Id, Int](i => out += s"sink $i": Unit)

    test("t |+| nullTracer and nullTracer |+| t are t itself") {
        val t = sink(ListBuffer.empty)
        val n = ContraTracer.nullTracer[Id, Int]
        assert(((t |+| n) eq t, (n |+| t) eq t, (n |+| n) eq n) == (true, true, true))
    }

    test("Monoid.combineAll over nullTracers and one tracer is that tracer") {
        val t = sink(ListBuffer.empty)
        val n = ContraTracer.nullTracer[Id, Int]
        assert(List(n, t, n, n).combineAll eq t)
    }

    test("a squelching leg's stage runs once an emitting stage follows the fan-out") {
        // `traceWith` alone never runs a squelching leg's stage; an emitting arrow after the
        // fan-out does, so dropping that leg from `|+|` would change what this tracer does.
        val out = ListBuffer.empty[String]
        val staged = ContraTracer
            .nullTracer[Id, Int]
            .contramapM[Int](i => { out += s"stage $i"; i }: Id[Int])
        val after = TracerA.emit[Id, Unit](_ => out += "after": Unit)
        ContraTracer.fromArrow((sink(out) |+| staged).runTracer >>> after).traceWith(1)
        assert(out.toList == List("sink 1", "stage 1", "after"))
    }

    test("a tracer derived from nullTracer is combined in full") {
        val t = sink(ListBuffer.empty)
        val derived = ContraTracer.nullTracer[Id, Int].contramap[Int](identity)
        assert(!((t |+| derived) eq t))
    }
}
