package org.kokho.scheduling.experiments

import java.io.{File, FileWriter, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
trait PerformanceAnalyzer {

  val consoleWriter = new PrintWriter(System.out, true)

  val genBuilder:TaskGeneratorBuilder = TaskGeneratorBuilder().setProbHi(0.4)

  def baseDir = "e:/Scheduling Experiments/"
  def outputDir: String

  def prepareWriter(): FileWriter = {
    val outputFile = new File(outputDir + "/" + generateFileName())
    new FileWriter(outputFile)
  }

  def generateFileName(): String = {
    val today = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("YYYYMMdd'at'HH_mm_ssSSSS'.analysis'")
    format.format(today)
  }


}
