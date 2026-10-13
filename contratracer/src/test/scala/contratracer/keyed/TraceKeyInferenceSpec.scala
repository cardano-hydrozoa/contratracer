package contratracer.keyed

import cats.Id
import contratracer.{ContraTracer, KeyedTracer, TraceKey}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer

/** What `KeyedTracer.traceWith[E <: A](e: => E)(using TraceKey[K, E])` infers for `E`, and so which
  * key it finds, for the event shapes Scala 3 offers. Keys are strings naming the instance that was
  * chosen.
  */
object TraceKeyInference {

    /** A sealed trait with case classes and case objects. */
    sealed trait Sealed
    object Sealed {
        final case class Produced(n: Int) extends Sealed
        object Produced { given TraceKey[String, Produced] = TraceKey.const("Produced") }
        case object Stopped extends Sealed {
            given TraceKey[String, Stopped.type] = TraceKey.const("Stopped")
        }
        final case class Unkeyed(n: Int) extends Sealed
    }

    /** The same events as a Scala 3 enum. */
    enum Enum:
        case Produced(n: Int)
        case Stopped

    object EnumKeys {
        given TraceKey[String, Enum.Produced] = TraceKey.const("Enum.Produced")
        given TraceKey[String, Enum] = TraceKey.const("Enum")
    }

    /** A sealed trait with a fallback for every subtype, beside per-case instances. */
    sealed trait WithFallback
    object WithFallback {
        given fallback[E <: WithFallback]: TraceKey[String, E] = TraceKey.const("fallback")
        final case class Produced(n: Int) extends WithFallback
        object Produced { given TraceKey[String, Produced] = TraceKey.const("Produced") }
        final case class Other(n: Int) extends WithFallback
    }

    def recorder[A](keys: ListBuffer[String]): KeyedTracer[Id, String, A] =
        KeyedTracer(_ => true, ContraTracer.emit[Id, (String, A)]((k, _) => keys += k: Unit))
}

class TraceKeyInferenceSpec extends AnyFunSuite {
    import TraceKeyInference.*

    /** Whether some error says no instance of `TraceKey[String, <tpe>]` was found. */
    private def missing(errors: List[scala.compiletime.testing.Error], tpe: String): Boolean =
        errors.exists(
          _.message
              .replaceAll("\\s+", "")
              .startsWith(
                s"Nogiveninstanceoftypecontratracer.TraceKey[String,contratracer.keyed.$tpe]"
              )
        )

    test("sealed trait: a case class and a case object each find their own instance") {
        val keys = ListBuffer.empty[String]
        val t = recorder[Sealed](keys)
        t.traceWith(Sealed.Produced(1))
        t.traceWith(Sealed.Stopped)
        assert(keys.toList == List("Produced", "Stopped"))
    }

    test("sealed trait: a case without an instance does not compile") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          "TraceKeyInference.recorder[TraceKeyInference.Sealed](ListBuffer.empty)" +
              ".traceWith(TraceKeyInference.Sealed.Unkeyed(1))"
        )
        assert(missing(errors, "TraceKeyInference.Sealed.Unkeyed"), errors.map(_.message))
    }

    test("sealed trait: if/else widens to the parent, which has no instance") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          "TraceKeyInference.recorder[TraceKeyInference.Sealed](ListBuffer.empty).traceWith(" +
              "if true then TraceKeyInference.Sealed.Produced(1) else TraceKeyInference.Sealed.Stopped)"
        )
        assert(missing(errors, "TraceKeyInference.Sealed"), errors.map(_.message))
    }

    test("enum: a case's apply returns the enum type, so a per-case instance is unreachable") {
        import EnumKeys.given
        val keys = ListBuffer.empty[String]
        val t = recorder[Enum](keys)
        t.traceWith(Enum.Produced(1))
        t.traceWith(Enum.Stopped)
        // `new` keeps the case type, and so does an explicit type argument over a cast.
        t.traceWith(new Enum.Produced(2))
        t.traceWith[Enum.Produced](Enum.Produced(3).asInstanceOf[Enum.Produced])
        assert(keys.toList == List("Enum", "Enum", "Enum.Produced", "Enum.Produced"))
    }

    test("enum: without an instance for the enum type, a case does not compile") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          "{ given TraceKey[String, TraceKeyInference.Enum.Produced] = TraceKey.const(\"p\");" +
              " TraceKeyInference.recorder[TraceKeyInference.Enum](ListBuffer.empty)" +
              ".traceWith(TraceKeyInference.Enum.Produced(1)) }"
        )
        assert(missing(errors, "TraceKeyInference.Enum"), errors.map(_.message))
    }

    test("a generic fallback instance loses to a per-case instance and covers the rest") {
        val keys = ListBuffer.empty[String]
        val t = recorder[WithFallback](keys)
        t.traceWith(WithFallback.Produced(1))
        t.traceWith(WithFallback.Other(2))
        t.traceWith(if keys.isEmpty then WithFallback.Other(3) else WithFallback.Produced(4))
        assert(keys.toList == List("Produced", "fallback", "fallback"))
    }
}
