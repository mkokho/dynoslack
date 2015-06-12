package org.kokho.scheduling.experiments

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
object TestB extends App with PerformanceAnalyzer {

  override val outputDir = baseDir +  "/TestB"

  println(
    """
      |Evaluating effects of Task Mixtures with Varying prob(HI)
      |Packing heuristic: worst fit
      |Utilization bound: 0.4
      |Probability of c^low: 0.9
      |Cores: 8
      | """.stripMargin)

  def getGenerator(prob: Double) = genBuilder.setProbHi(prob).build()

  for (prob <- 0.2.to(0.8, 0.1)) {
    val experiment = new Experiment(
      generator = getGenerator(prob),
      utilizationBound = 0.4,
      cores = 8,
      repetition = 100,
      duration = 20000,
      writer = prepareWriter())

    print(s"Running experiment with prob set to $prob... ")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

  println("All test has completed.")
}
