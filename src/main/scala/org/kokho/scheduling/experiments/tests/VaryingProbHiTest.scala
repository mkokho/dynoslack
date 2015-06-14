package org.kokho.scheduling.experiments.tests

import org.kokho.scheduling.experiments.Experiment

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/14/2015.
 */
object VaryingProbHiTest extends PerformanceAnalyzer with App{
  override val outputDir = baseDir +  "/VaryingProbHi"

  def getGenerator(prob: Double) = genBuilder.setProbHi(prob).build()

  def experiment(probHi: Double) = {
    val experiment = new Experiment(
      generator = genBuilder
        .setProbHi(probHi)
        .setPeriodBound(50, 200)
        .setRatioWCET(8)
        .setMaxER(200)
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

  for (probHi <- 0.2.to(0.8, 0.1)) {
    experiment(probHi)
  }

  println("All test has completed.")
}

