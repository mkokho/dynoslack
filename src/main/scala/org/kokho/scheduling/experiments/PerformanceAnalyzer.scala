package org.kokho.scheduling.experiments

import java.io.{File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
object PerformanceAnalyzer extends App {

  val consoleWriter = new PrintWriter(System.out, true)

  val outputDir = new File("e:/Scheduling Experiments/")
  require(outputDir.exists())
  require(outputDir.isDirectory)

  private def generateFileName(): String = {
    val today = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("YYYYMMdd'at'HH_mm_ssSSSS'.analysis'")
    format.format(today)
  }

  def experimentOne(): Unit = {
    val outputFile = new File(outputDir.getAbsolutePath + "/" + generateFileName())
    val fileWriter = new FileWriter(outputFile)

    val generator = new TaskGenerator(
      probHi = 0.8,
      probLowJob = 0.9,
      periodMin = 50,
      periodMax = 200,
      maxERPoints = 10
    )

    val experiment = new Experiment(
      generator = generator,
      utilizationBound = 0.8,
      cores = 8,
      repetition = 100,
      duration = 20000,
      writer = fileWriter)

    //

    Await.result(experiment.run(), Duration.Inf)
  }

  experimentOne()
}
