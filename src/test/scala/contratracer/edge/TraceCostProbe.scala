package contratracer.edge

import cats.effect.IO
import contratracer.ContraTracer
import java.lang.management.ManagementFactory

/** Bytes allocated and nanoseconds spent per trace, call and run together, for a few shapes in IO,
  * run on this thread through `syncStep`. A coarse comparison between two builds of the library,
  * not a benchmark: no forks, no blackholes beyond a counter.
  *
  * `java -cp <Test/fullClasspath> contratracer.edge.TraceCostProbe [traces]`.
  */
object TraceCostProbe {

    private val threads = ManagementFactory.getThreadMXBean match {
        case t: com.sun.management.ThreadMXBean => t
        case other                              => sys.error(s"no allocation counter on $other")
    }

    def main(args: Array[String]): Unit = {
        val traces = args.headOption.map(_.toInt).getOrElse(2000000)
        var emitted = 0L
        def leaf = ContraTracer.emit[IO, Int](_ => IO.delay(emitted += 1))
        val shapes = List(
          "emit" -> leaf,
          "contramap" -> leaf.contramap[Int](_ + 1),
          "L |+| R" -> (leaf |+| leaf),
          "(L |+| R) |+| S" -> ((leaf |+| leaf) |+| leaf),
          "L.contramap |+| R" -> (leaf.contramap[Int](_ + 1) |+| leaf)
        )
        def run(t: ContraTracer[IO, Int], n: Int): Unit = {
            var i = 0
            while i < n do {
                t.traceWith(i).syncStep(Int.MaxValue).unsafeRunSync(): Unit
                i += 1
            }
        }
        shapes.foreach((_, t) => run(t, traces)) // warm-up
        val self = Thread.currentThread.threadId
        shapes.foreach { (name, t) =>
            val bytes0 = threads.getThreadAllocatedBytes(self)
            val t0 = System.nanoTime()
            run(t, traces)
            val ns = (System.nanoTime() - t0).toDouble / traces
            val bytes = (threads.getThreadAllocatedBytes(self) - bytes0).toDouble / traces
            println(f"$name%-20s $bytes%7.1f B/trace $ns%7.1f ns/trace")
        }
        println(s"($traces traces per shape, $emitted emissions in all)")
    }
}
