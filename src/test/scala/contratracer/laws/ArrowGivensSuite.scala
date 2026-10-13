package contratracer.laws

import cats.arrow.*
import contratracer.*
import contratracer.laws.Recorded.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/** Every `compose` [[TracerA]] offers, named or summoned, must compose. Scala 3.3 resolves a summon
  * to the most specific given that fits and Scala 3.7 to the most general, so the library keeps a
  * single instance; each class is summoned here to check every summon lands on it.
  */
class ArrowGivensSuite extends AnyFunSuite:
    private val emitter: TA[Int, Unit] = TracerA.emit[W, Int](a => emitted(1, a))
    private val plusOne: TA[Int, Int] =
        TracerA.effect[W, Int, Int](a => effected(2, a).map(_ => a + 1))

    private val instances: List[(String, Compose[TA])] = List(
      "named instance" -> TracerA.arrowChoice[W],
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

/** cats' Arrow laws against whatever `summon[Arrow]` returns, which is what an `Arrow` user gets.
  */
class ArrowGivensLawsSuite extends AnyFunSuite with FunSuiteDiscipline with Checkers:
    checkAll(
      "summoned Arrow, Emits+tag",
      ArrowRules.arrow(Obs.Emits, true, summon[Arrow[TA]])
    )
