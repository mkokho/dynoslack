package org.kokho.scheduling.experiments.tests

import org.kokho.scheduling.experiments.{Experiment, NotRandomTaskGenerator}
import org.kokho.scheduling.rts.multicritical.{HiCriticalTask, LoCriticalTask, MulticriticalTask}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
object TestPlayground extends PerformanceAnalyzer with App {

  def taskAccept(task: MulticriticalTask): Boolean = task match {
    case loTask: LoCriticalTask =>
      loTask.utilization > 0.4 && loTask.period < 80
    case hiTask: HiCriticalTask =>
      hiTask.period > 120
  }

  override val outputDir = baseDir + "/TestPlayground"

  def experiment() = {

    val gen = new NotRandomTaskGenerator(
      genBuilder
        .setProbHi(0.5)
        .setPeriodBound(50, 200)
        .setRatioWCET(4)
        .setMaxER(10)
        .setUtilization(0.05, 0.5)
        .setProbLowJob(0.0)
        .build(),
      taskAccept
    )

    val experiment = new Experiment(
      generator = gen,
      utilizationBound = 0.6,
      cores = 8,
      repetition = 100,
      duration = 20000,
      writer = prepareWriter())

    print(s"Running experiment...")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

  experiment()

  println("All test has completed.")
}
