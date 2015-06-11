package org.kokho.scheduling.experiments

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
object PerformanceAnalyzer extends App{

  val generator = new TaskGenerator(0.8, 0.9)
  val experiment = new Experiment(generator, 0.8, 4)

  0.until(20) foreach {_ =>
    experiment.run()
    println()
  }
}
