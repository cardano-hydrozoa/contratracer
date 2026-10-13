package contratracer.effect

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import contratracer.ContraTracer
import java.util.Base64
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

class RecordingSpec extends AnyFunSuite {

    private val Fibers = 32
    private val PerFiber = 1000

    /** `Fibers` fibers in parallel, each tracing `(fiber, i)` for `i` from 0 to `perFiber - 1`. */
    private def hammer(t: ContraTracer[IO, (Int, Int)], perFiber: Int, cede: Boolean): IO[Unit] =
        (0 until Fibers).toList.parTraverse_(f =>
            (0 until perFiber).toList.traverse_(i => t.traceWith((f, i)) >> IO.cede.whenA(cede))
        )

    /** Every event arrived once, and each fiber's events in the order it traced them. */
    private def complete(seen: Vector[(Int, Int)], perFiber: Int): Boolean =
        seen.size == Fibers * perFiber &&
            seen.groupMap(_._1)(_._2).values.forall(_ == (0 until perFiber).toVector)

    test("make: 32 parallel fibers lose nothing, and each fiber's order is kept") {
        val seen = Recording
            .make[IO, (Int, Int)]
            .flatMap((t, read) => hammer(t, PerFiber, cede = false) >> read)
            .unsafeRunSync()
        assert(complete(seen, PerFiber), s"recorded ${seen.size} of ${Fibers * PerFiber}")
    }

    /** A buffer that reads, yields, then writes, as an unsynchronised one may interleave. */
    private val racyBuffer: IO[(ContraTracer[IO, (Int, Int)], IO[Vector[(Int, Int)]])] = IO {
        var buffer = Vector.empty[(Int, Int)]
        val t = ContraTracer.emit[IO, (Int, Int)](e =>
            IO(buffer).flatMap(b => IO.cede >> IO { buffer = b :+ e })
        )
        (t, IO(buffer))
    }

    test("control: under TestControl a read-yield-write buffer loses events that make keeps") {
        def run(capture: IO[(ContraTracer[IO, (Int, Int)], IO[Vector[(Int, Int)]])]) =
            TestControl
                .executeEmbed(capture.flatMap((t, read) => hammer(t, 10, cede = true) >> read))
                .unsafeRunSync()
        val racy = run(racyBuffer)
        val _ = assert(!complete(racy, 10), "the racy buffer lost nothing")
        info(s"the racy buffer kept ${racy.size} of ${Fibers * 10}")
        assert(complete(run(Recording.make[IO, (Int, Int)]), 10))
    }

    /** Three fibers that trace in turns, yielding between events. */
    private val interleaved: IO[Vector[(Int, Int)]] =
        Recording.make[IO, (Int, Int)].flatMap { (t, read) =>
            (0 until 3).toList.parTraverse_(f =>
                (0 until 5).toList.traverse_(i => t.traceWith((f, i)) >> IO.cede)
            ) >> read
        }

    private def seed(n: Long): String = Base64.getEncoder.encodeToString(n.toString.getBytes)

    private def underSeed(n: Long): Vector[(Int, Int)] =
        TestControl.executeEmbed(interleaved, seed = Some(seed(n))).unsafeRunSync()

    test("under TestControl one seed gives one recorded order, and seeds differ") {
        val orders = (1L to 20L).map(n => n -> underSeed(n)).toMap
        orders.foreach((n, order) => assert(underSeed(n) == order, s"seed $n did not replay"))
        orders.values.foreach(order =>
            assert(order.groupMap(_._1)(_._2).values.forall(_ == (0 until 5).toVector))
        )
        val distinct = orders.values.toSet.size
        info(s"distinct recorded orders over 20 seeds: $distinct")
        assert(distinct > 1, "the seed did not change the interleaving")
    }

    test("awaiting: the waiter wakes on the first match, which is already recorded") {
        val program = for {
            recording <- Recording.awaiting[IO, Int](_ > 2)
            (t, read, first) = recording
            waiter <- first.flatMap(n => read.map(seen => (n, seen))).start
            _ <- (1 to 5).toList.traverse_(n => t.traceWith(n))
            woke <- waiter.joinWithNever
            all <- read
        } yield (woke, all)
        val ((n, seenAtWake), all) = TestControl.executeEmbed(program).unsafeRunSync()
        assert((n, seenAtWake.contains(3), all) == (3, true, Vector(1, 2, 3, 4, 5)))
    }

    test("awaiting: the wait blocks until a match is traced, from another fiber") {
        val program = for {
            recording <- Recording.awaiting[IO, String](_ == "ready")
            (t, _, first) = recording
            woke <- Ref[IO].of(false)
            waiting <- first.flatTap(_ => woke.set(true)).start
            _ <- (t.traceWith("starting") >> IO.sleep(1.second) >> t.traceWith("ready")).start
            early <- IO.sleep(500.millis) >> woke.get
            got <- waiting.joinWithNever
        } yield (early, got)
        assert(TestControl.executeEmbed(program).unsafeRunSync() == (false, "ready"))
    }
}
