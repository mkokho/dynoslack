package org.kokho.scheduling.experiments.tests

import org.kokho.scheduling.experiments.Experiment

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
object TestPlayground extends PerformanceAnalyzer with App {

  override val outputDir = baseDir +  "/TestPlayground"

  def experiment() = {
    val experiment = new Experiment(
      generator = genBuilder
        .setProbHi(0.2)
        .setPeriodBound(50, 200)
        .setRatioWCET(8)
        .setMaxER(10)
        .setUtilization(0.05, 0.5)
        .setProbLowJob(0.9)
        .build(),
      utilizationBound = 0.4,
      cores = 8,
      repetition = 100,
      duration = 40000,
      writer = prepareWriter())

    print(s"Running experiment...")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

  experiment()

  println("All test has completed.")
}
