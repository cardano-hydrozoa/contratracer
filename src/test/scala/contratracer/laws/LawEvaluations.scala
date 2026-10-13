package contratracer.laws

import org.scalactic.anyvals.PosInt
import org.scalatest.prop.Configuration

/** The number of evaluations each law gets in a discipline suite: 1,000 per law, against
  * ScalaTest's default of 10, which can miss a law that fails only for some generated tracers.
  *
  * The system property `contratracer.laws.evaluations` overrides it. Before a release, run the law
  * suites at 5,000 on three seeds: `sbt -Dcontratracer.laws.evaluations=5000 "testOnly
  * contratracer.laws.*"`, three times, each run with its own random seed.
  */
trait LawEvaluations extends Configuration:
    override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(
          minSuccessful = PosInt
              .from(sys.props.get("contratracer.laws.evaluations").fold(1000)(_.toInt))
              .getOrElse(sys.error("contratracer.laws.evaluations must be a positive integer"))
        )
