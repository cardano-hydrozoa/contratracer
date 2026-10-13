package hydrozoa.lib.logging.laws

import cats.arrow.*
import hydrozoa.lib.logging.*
import hydrozoa.lib.logging.laws.Recorded.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** Every `compose` [[TracerA]] offers, named or summoned, must compose. A summon on Scala 3.3
  * reaches only the most specific given (ArrowChoice); Scala 3.7 prefers the most general one that
  * fits, so `summon[Arrow]` reaches the Arrow given. Both are checked here by name.
  */
class ArrowGivensSuite extends AnyFunSuite:
    private val emitter: TA[Int, Unit] = TracerA.emit[W, Int](a => emitted(1, a))
    private val plusOne: TA[Int, Int] =
        TracerA.effect[W, Int, Int](a => effected(2, a).map(_ => a + 1))

    private val instances: List[(String, Compose[TA])] = List(
      "named Category given" -> TracerA.given_Category_TracerA[W],
      "named Arrow given" -> TracerA.given_Arrow_TracerA[W],
      "named ArrowChoice given" -> TracerA.given_ArrowChoice_TracerA[W],
      "summoned Compose" -> summon[Compose[TA]],
      "summoned Category" -> summon[Category[TA]],
      "summoned Arrow" -> summon[Arrow[TA]],
      "summoned ArrowChoice" -> summon[ArrowChoice[TA]]
    )

    instances.foreach { (name, instance) =>
        test(s"compose and andThen terminate and agree: $name") {
            val viaCompose = instance.compose(emitter, plusOne)
            val viaAndThen = instance.andThen(plusOne, emitter)
            val expected = List(Ev.Eff(2, "41"), Ev.Emit(1, "42"))
            val composeRan = log(compiled(viaCompose)(using monadW)(41))
            val andThenRan = log(compiled(viaAndThen)(using monadW)(41))
            assert(
              isEmitting(viaCompose) && isEmitting(viaAndThen) &&
                  composeRan == expected && andThenRan == expected
            )
        }
    }

/** cats' Arrow laws against the Arrow given by name: its `second`, `dimap` and `andThen` route
  * through its own `compose`, which no summon on Scala 3.3 reaches.
  */
class ArrowGivensLawsSuite extends AnyFunSuite with FunSuiteDiscipline with Checkers:
    checkAll(
      "named Arrow given, Emits+tag",
      ArrowRules.arrow(Obs.Emits, true, TracerA.given_Arrow_TracerA[W])
    )
