package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.{HiCriticalTask, LoCriticalTask, MulticriticalTask}

import scala.collection.mutable
import scala.util.Random

/**
 * Generator of random tasks
 *
 * @param probHi - probability of generating HI critical task
 * @param probLowJob - probability of taking low WCET by a HI critical task
 * @param maxERPoints - maximal number of early release point a task can have
 * @param periodMin - lower bound on the period of every generated task
 * @param periodMax - upper bound on the period of every generated task
 */
class TaskGenerator(val probHi: Double,
                    val probLowJob: Double,
                    val maxERPoints: Int,
                    val periodMin: Int,
                    val periodMax: Int) {

  /**
   * Minimal utilization of a task
   */
  val MIN_UTILIZATION = 0.05

  /**
   * Maximal utilization of a task
   */
  val MAX_UTILIZATION = 0.5

  /**
   * A high WCET of a HI critical task is this times greater that low WCET
   */
  val MAX_OVERPROVISIONING = 8

  /**
   * Utilization and WCET of a task are real numbers
   * We scale and round these number to get integers
   */
  private val SCALE = 3

  private def scaled(d: Double): Int = Math.floor(d * SCALE).toInt

  private def debugMistake(period: Int, utilization: Double, execution: Double): Unit = {
    val scaledExecution = scaled(execution)
    val scaledPeriod = scaled(period)
    val roundingMistake: Double = utilization - scaledExecution.toDouble / scaledPeriod
    assert(roundingMistake <= 0.01, s"Roubding mistake is greater than 0.01: $roundingMistake")
  }

  def generateMulticriticalTask(): MulticriticalTask = {
    if (Random.nextDouble() > this.probHi)
      generateLoTask(MIN_UTILIZATION, MAX_UTILIZATION)
    else generateHiTask(MIN_UTILIZATION, MAX_UTILIZATION)
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

    //our jobs accept integer parameters.
    //we scale to reduce rounding mistake
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
    val uRatio = randomInt(1, MAX_OVERPROVISIONING + 1)

    //given ration and HI utilization we compute LO utilization
    val uLo = uHi / uRatio

    //given utilization and period we compute execution requirements
    val hiWcet = uHi * period
    val loWcet = uLo * period

    debugMistake(period, uHi, hiWcet)
    HiCriticalTask(scaled(period), scaled(loWcet), scaled(hiWcet), loJob(probLowJob))
  }


  private def loJob(prob: Double): Int => Boolean = {
    val m = mutable.Map.empty[Int, Boolean]
    m.getOrElseUpdate(_, Random.nextDouble() > prob)
  }

  /**
   * Generates a random number between $lo (inclusive) and $hi (exclusive)
   */
  private def randomInt(lo: Int, hi: Int) = Random.nextInt(hi - lo) + lo

  private def randomDouble(lo: Double, hi: Double) = lo + (hi - lo) * Random.nextDouble()

  private def randomStream(lo: Int, hi: Int, acc: Stream[Int]): Stream[Int] =
    randomInt(lo, hi) match {
      case r if acc.contains(r) => randomStream(lo, hi, acc)
      case r => r #:: randomStream(lo, hi, r #:: acc)
    }

  private def randomPeriod() = randomInt(periodMin, periodMax + 1)

  private def randomEarlyReleases(min: Int, max: Int): Seq[Int] = {
    if (max - min <= maxERPoints) min.until(max)
    else randomStream(min, max, Stream.empty).take(maxERPoints)
  }

  override def toString: String = s"TaskGenerator(probHi: $probHi, probLowJob: $probLowJob," +
    s"maxERPotins: $maxERPoints, period: between $periodMin and $periodMax)"
}
