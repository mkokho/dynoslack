package org.kokho.scheduling.experiments.tests

import org.kokho.scheduling.experiments.{Experiment, FixedTaskGenerator}
import org.kokho.scheduling.rts.multicritical.{HiCriticalTask, LoCriticalTask}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/13/2015.
 */
object TestShortPeriods extends PerformanceAnalyzer with App{

  override val outputDir = baseDir +  "/TestShortPeriods"

  /** Default Parameters are:
    probHI            0.5,
    low jobs:         0.9,
    ER points:        10,
    period:           [50, 200]
    utilization :     [0.05, 0.5]
    hiWCET/loWCET:    4
    */


  def periodExperiment(periodMin: Int, periodMax: Int) = {
    val experiment = new Experiment(
      generator = genBuilder
        .setPeriodBound(periodMin, periodMax)
//        .setRatioWCET(3)
        .setMaxER(200)
        .setProbLowJob(0.0)
        .build(),
      utilizationBound = 0.8,
      cores = 3,
      repetition = 1,
      duration = 1000,
      writer = prepareWriter())

    print(s"Running experiment with periods set to [$periodMin, $periodMax]... ")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

  def fixedExperiment(): Unit = {
    val taskset = Seq(
      HiCriticalTask(125, 8, 35),
      HiCriticalTask(210, 7, 22),
      LoCriticalTask(125, 31),
      HiCriticalTask(245, 6, 27),
      HiCriticalTask(130, 4, 19),
      LoCriticalTask(165, 47),
      LoCriticalTask(125, 37),
      HiCriticalTask(195, 36, 72),
      LoCriticalTask(195, 55),
      HiCriticalTask(230, 15, 15),
      HiCriticalTask(215, 22, 45))


    val experiment = new Experiment(
      generator = new FixedTaskGenerator(taskset),
      utilizationBound = 0.8,
      cores = 3,
      repetition = 1,
      duration = 1000,
      writer = prepareWriter())

    print(s"Running experiment with fixed task set ... ")
    Await.result(experiment.run(), Duration.Inf)
    println(" done.")
  }

//  periodExperiment(20,50)
  fixedExperiment()

  println("All test has completed.")
}

