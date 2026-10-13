package contratracer.edge

import cats.data.{Kleisli, State, StateT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.{Eval, Id, Monad}
import scala.collection.mutable.ListBuffer

/** An ordered record of what ran, shared by a tracer under test and the test driving it. */
final class PhaseLog {
    private val entries = ListBuffer.empty[String]
    def apply(entry: String): Unit = entries += entry
    def drain(): List[String] = { val out = entries.toList; entries.clear(); out }
}

/** One monad the edge-case suites run every shape in: how to suspend a side effect in it, and how
  * to run a traced value of it to completion.
  */
trait Runner[M[_]] {
    def name: String
    given monad: Monad[M]
    def delay(thunk: => Unit): M[Unit]
    def run(m: M[Unit]): Unit
}

object Runner {
    type StateIO[A] = StateT[IO, Int, A]
    type ReaderIO[A] = Kleisli[IO, String, A]
    type PureState[A] = State[Int, A]

    private val idMonad: Monad[Id] = summon[Monad[Id]]
    private val evalMonad: Monad[Eval] = summon[Monad[Eval]]
    private val ioMonad: Monad[IO] = summon[Monad[IO]]
    private val stateMonad: Monad[StateIO] = summon[Monad[StateIO]]
    private val readerMonad: Monad[ReaderIO] = summon[Monad[ReaderIO]]
    private val pureStateMonad: Monad[PureState] = summon[Monad[PureState]]

    val id: Runner[Id] = new Runner[Id] {
        val name = "Id"
        given monad: Monad[Id] = idMonad
        def delay(thunk: => Unit): Id[Unit] = thunk
        def run(m: Id[Unit]): Unit = m
    }

    val eval: Runner[Eval] = new Runner[Eval] {
        val name = "Eval"
        given monad: Monad[Eval] = evalMonad
        def delay(thunk: => Unit): Eval[Unit] = Eval.always(thunk)
        def run(m: Eval[Unit]): Unit = m.value
    }

    val io: Runner[IO] = new Runner[IO] {
        val name = "IO"
        given monad: Monad[IO] = ioMonad
        def delay(thunk: => Unit): IO[Unit] = IO.delay(thunk)
        def run(m: IO[Unit]): Unit = m.unsafeRunSync()
    }

    val stateT: Runner[StateIO] = new Runner[StateIO] {
        val name = "StateT[IO]"
        given monad: Monad[StateIO] = stateMonad
        def delay(thunk: => Unit): StateIO[Unit] = StateT.liftF(IO.delay(thunk))
        def run(m: StateIO[Unit]): Unit = m.runA(0).unsafeRunSync()
    }

    val kleisli: Runner[ReaderIO] = new Runner[ReaderIO] {
        val name = "Kleisli[IO]"
        given monad: Monad[ReaderIO] = readerMonad
        def delay(thunk: => Unit): ReaderIO[Unit] = Kleisli.liftF(IO.delay(thunk))
        def run(m: ReaderIO[Unit]): Unit = m.run("env").unsafeRunSync()
    }

    /** `State` is `StateT` over `Eval`: the stack hydrozoa's mock Cardano backend traces into. */
    val state: Runner[PureState] = new Runner[PureState] {
        val name = "State"
        given monad: Monad[PureState] = pureStateMonad
        def delay(thunk: => Unit): PureState[Unit] = State(s => { thunk; (s, ()) })
        def run(m: PureState[Unit]): Unit = m.runA(0).value
    }
}
