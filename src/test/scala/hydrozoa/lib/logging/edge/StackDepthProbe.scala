package hydrozoa.lib.logging.edge

import java.lang.management.ManagementFactory

/** Finds, for every deep shape in every monad, the first composition depth whose trace overflows a
  * thread with the JVM's default stack. It makes three passes: the first runs cold, the later ones
  * after the JIT has compiled the hot frames, which shrinks them.
  *
  * Run outside sbt so the JVM flags are known:
  * `java -cp <Test/fullClasspath> hydrozoa.lib.logging.edge.StackDepthProbe [stackKiB] [cap]`.
  */
object StackDepthProbe {
    def main(args: Array[String]): Unit = {
        val stackBytes = args.headOption.map(_.toLong * 1024).getOrElse(0L)
        val cap = args.lift(1).map(_.toInt).getOrElse(1 << 20)
        val jvm = System.getProperty("java.version")
        val flags = ManagementFactory.getRuntimeMXBean.getInputArguments
        val stack = if stackBytes == 0 then "the JVM default" else s"$stackBytes bytes"
        println(s"JVM $jvm, flags $flags, thread stack $stack, cap $cap")
        val runs = for
            pass <- 1 to 3
            shape <- DeepShapes.Shape.values
            target <- DeepShapes.targets
        yield (pass, shape, target)
        runs.foreach { (pass, shape, target) =>
            val started = System.nanoTime()
            val result = DeepShapes.firstOverflow(target, shape, stackBytes, cap) match {
                case Some((depth, phase)) => s"overflows at depth $depth (in $phase)"
                case None                 => s"no overflow up to $cap"
            }
            val ms = (System.nanoTime() - started) / 1000000
            println(f"pass $pass ${shape.label}%-20s ${target.name}%-5s $result ($ms ms)")
        }
    }
}
