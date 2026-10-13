package hydrozoa.lib.logging.edge

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.lib.logging.ContraTracer

/** Heap retained while `traceTraversable` walks a structure of `n` elements, sampled after a full
  * GC: before tracing, when `traceWith` returns, and at each quarter of the emissions.
  *
  * `java -Xmx2g -cp <Test/fullClasspath> hydrozoa.lib.logging.edge.TraverseMemoryProbe [n]`.
  */
object TraverseMemoryProbe {

    private def usedMiB(): Long = {
        System.gc(); System.gc()
        val rt = Runtime.getRuntime
        (rt.totalMemory - rt.freeMemory) / (1024 * 1024)
    }

    /** Samples at each quarter of `n` emissions; the sink calls `tick` once per element. */
    private final class Sampler(n: Int) {
        private var seen = 0
        val samples = scala.collection.mutable.ListBuffer.empty[String]
        def tick(): Unit = {
            seen += 1
            if seen % (n / 4) == 0 then samples += s"${seen * 100 / n}%=${usedMiB()}"
        }
    }

    private def report(label: String, n: Int)(trace: (() => Unit) => () => Unit): Unit = {
        val sampler = Sampler(n)
        val before = usedMiB()
        val run = trace(() => sampler.tick())
        val returned = usedMiB()
        run()
        val after = usedMiB()
        println(
          f"$label%-26s MiB used: before $before, returned $returned, " +
              s"${sampler.samples.mkString(", ")}, after $after"
        )
    }

    def main(args: Array[String]): Unit = {
        val n = args.headOption.map(_.toInt).getOrElse(1000000)
        println(s"n = $n, max heap ${Runtime.getRuntime.maxMemory / (1024 * 1024)} MiB")
        val list = List.range(0, n)
        report("Id List (held)", n) { tick =>
            val t = ContraTracer.emit[Id, Int](_ => tick())
            () => t.traceTraversable[List].traceWith(list)
        }
        report("IO List (held)", n) { tick =>
            val traced = ContraTracer
                .emit[IO, Int](_ => IO.delay(tick()))
                .traceTraversable[List]
                .traceWith(list)
            () => traced.unsafeRunSync()
        }
        report("Id LazyList (generated)", n) { tick =>
            val t = ContraTracer.emit[Id, Int](_ => tick())
            () => t.traceTraversable[LazyList].traceWith(LazyList.range(0, n))
        }
        report("IO LazyList (generated)", n) { tick =>
            val traced = ContraTracer
                .emit[IO, Int](_ => IO.delay(tick()))
                .traceTraversable[LazyList]
                .traceWith(LazyList.range(0, n))
            () => traced.unsafeRunSync()
        }
        println(s"(the held List of $n is ${list.length} elements, retained throughout)")
    }
}
