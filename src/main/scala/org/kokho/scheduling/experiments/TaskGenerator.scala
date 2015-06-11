package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.{HiCriticalTask, LoCriticalTask, MulticriticalTask}

import scala.util.Random

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
class TaskGenerator(val ratioLoHi: Double) {
  private val MAX_ER_POINTS = 20

  /**
   * Utilization and WCET of a task are real numbers
   * We scale and round these number to get integers
   */
  private val SCALE = 10

  def scaled(d: Double): Int = Math.floor(d * SCALE).toInt

  private def debugMistake(period:Int, utilization: Double, execution: Double): Unit = {
    val scaledExecution = scaled(execution)
    val scaledPeriod = scaled(period)
    val roundingMistake: Double = utilization - scaledExecution.toDouble / scaledPeriod
    assert(roundingMistake <= 0.01, s"Roubding mistake is greater than 0.01: $roundingMistake")
  }
  
  /**
   * Generates a random number between $lo (inclusive) and $hi (exclusive)
   */
  private def randomInt(lo: Int, hi: Int) = Random.nextInt(hi - lo) + lo

  private def randomDouble(lo: Double, hi: Double) = lo + (hi-lo) * Random.nextDouble()

  private def randomStream(lo: Int, hi: Int, acc:Stream[Int]): Stream[Int] =
    randomInt(lo, hi) match {
      case r if acc.contains(r) => randomStream(lo, hi, acc)
      case r => r #:: randomStream(lo, hi, r #:: acc)
    }

  private def randomPeriod() = randomInt(50, 200 + 1)

  private def randomEarlyReleases(min: Int, max: Int): Seq[Int] = {
    if (max - min <= MAX_ER_POINTS) min.until(max)
    else randomStream(min, max, Stream.empty).take(MAX_ER_POINTS)
  }

  def generateLoTask(uMin: Double, uMax: Double): LoCriticalTask = {
    require(uMin > 0)
    require(uMax <= 1)
    require(uMin < uMax)

    //period of the task
    val period = randomPeriod()
    
    //utilization of the task
    val utilization = randomDouble(uMin, uMax)

    //given utilization and period we compute execution requirements
    val wcet = utilization * period
    debugMistake(period, utilization, wcet)

    val wcetScaled = scaled(wcet)
    val periodScaled = scaled(period)

    val erPoints = randomEarlyReleases(wcetScaled, periodScaled).toList

    LoCriticalTask(periodScaled, wcetScaled, erPoints)
  }

  def generateHiTask(uMin: Double, uMax: Double): HiCriticalTask = {
    require(uMin > 0)
    require(uMax <= 1)
    require(uMin < uMax)

    //period of the task
    val period = randomPeriod()

    //HI utilization the task
    val uHi = randomDouble(uMin, uMax)

    //ration of HI utilization to LO utilization, i.e. uHI/uLo
    val uRatio = randomInt(1, 8 + 1)

    //given ration and HI utilization we compute LO utilization
    val uLo = uHi / uRatio

    //given utilization and period we compute execution requirements
    val hiWcet = uHi * period
    val loWcet = uLo * period

    debugMistake(period, uHi, hiWcet)
    HiCriticalTask(scaled(period), scaled(loWcet), scaled(hiWcet))
  }

  def generateMulticriticalTask(): MulticriticalTask = {
    //utilization bounds of the task
    val uMin = 0.05
    val uMax = 0.5
    if (Random.nextDouble() > this.ratioLoHi) generateLoTask(uMin, uMax)
    else generateHiTask(uMin, uMax)
  }

}
