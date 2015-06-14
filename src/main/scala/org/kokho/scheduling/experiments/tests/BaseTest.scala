package org.kokho.scheduling.experiments.tests

import org.kokho.scheduling.experiments.Experiment

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
object BaseTest extends PerformanceAnalyzer with App {

  override val outputDir = baseDir + "/BaseTest"

  def getGenerator(prob: Double) = genBuilder.setProbHi(prob).build()

  def experiment(utilization: Double) = {
    val experiment = new Experiment(
      generator = genBuilder
        .setProbHi(0.5)
        .setPeriodBound(50, 200)
        .setRatioWCET(8)
        .setMaxER(10)
        .setProbLowJob(0.9)
        .build(),
      utilizationBound = utilization,
      cores = 8,
      repetition = 100,
      duration = 40000,
      writer = prepareWriter())

    print(s"Running experiment...")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

  for (prob <- 0.5.to(0.8, 0.1)) {
    experiment(prob)
  }

  println("All test has completed.")
}
