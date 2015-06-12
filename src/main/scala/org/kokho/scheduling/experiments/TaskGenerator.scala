package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.{HiCriticalTask, LoCriticalTask, MulticriticalTask}

import scala.collection.mutable
import scala.util.Random

/**
 *  * Generator of random tasks
 *
 * @param probHi - probability of generating HI critical task
 * @param probLowJob - probability of taking low WCET by a HI critical task
 * @param maxERPoints - maximal number of early release point a task can have
 * @param periodMin - lower bound on the period of every generated task
 * @param periodMax - upper bound on the period of every generated task
 * @param utilizationMin
 * @param utilizationMax
 * @param ratioUhighToUlow - A high WCET of a HI critical task is this times greater that low WCET
 */
class TaskGenerator(val probHi: Double,
                    val probLowJob: Double,
                    val maxERPoints: Int,
                    val periodMin: Int,
                    val periodMax: Int,
                    val utilizationMin: Double,
                    val utilizationMax: Double,
                     val ratioUhighToUlow: Int) {

  require(utilizationMin > 0)
  require(utilizationMax <= 1)
  require(utilizationMin < utilizationMax)
  require(probHi >= 0 && probHi <= 1)
  require(maxERPoints >= 0)
  require(periodMin < periodMax)
  require(periodMin > 0)
  require(utilizationMin > 0)
  require(utilizationMax <= 1)

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
      generateLoTask()
    else
      generateHiTask()
  }

  def generateLoTask(): LoCriticalTask = {
    //period of the task
    val period = randomPeriod()

    //utilization of the task
    val utilization = randomDouble(utilizationMin, utilizationMax)

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

  def generateHiTask(): HiCriticalTask = {

    //period of the task
    val period = randomPeriod()

    //HI utilization the task
    val uHi = randomDouble(utilizationMin, utilizationMax)

    //ration of HI utilization to LO utilization, i.e. uHI/uLo
    val uRatio = randomInt(1, ratioUhighToUlow + 1)

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
    m.getOrElseUpdate(_, Random.nextDouble() <= prob)
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

case class TaskGeneratorBuilder(_probHi: Double = 0.5,
                                _probLowJob: Double = 0.9,
                                _maxERPoints: Int = 10,
                                _periodMin: Int = 50,
                                _periodMax: Int = 200,
                                _utilizationMin: Double = 0.05,
                                _utilizationMax: Double = 0.5,
                                _ratioHighWCETtoLowWCET: Int = 4) {


  def build() = new TaskGenerator(
    probHi = this._probHi,
    probLowJob = this._probLowJob,
    maxERPoints = this._maxERPoints,
    periodMin = this._periodMin,
    periodMax = this._periodMax,
    utilizationMin = this._utilizationMin,
    utilizationMax = this._utilizationMax,
    ratioUhighToUlow = this._ratioHighWCETtoLowWCET
  )

  def setProbHi(newProbHi: Double): TaskGeneratorBuilder =
    TaskGeneratorBuilder(
      _probHi = newProbHi,
      _probLowJob = this._probLowJob,
      _maxERPoints = this._maxERPoints,
      _periodMin = this._periodMin,
      _periodMax = this._periodMax,
      _utilizationMin = this._utilizationMin,
      _utilizationMax = this._utilizationMax,
      _ratioHighWCETtoLowWCET = this._ratioHighWCETtoLowWCET
    )

  def setProbLowJob(newProbLowJob: Double) = TaskGeneratorBuilder(
    _probLowJob = newProbLowJob,
    _probHi = this._probHi,
    _maxERPoints = this._maxERPoints,
    _periodMin = this._periodMin,
    _periodMax = this._periodMax,
    _utilizationMin = this._utilizationMin,
    _utilizationMax = this._utilizationMax,
    _ratioHighWCETtoLowWCET = this._ratioHighWCETtoLowWCET
  )

  def setPeriodBound(newPeriodMin: Int, newPeriodMax: Int) =
    TaskGeneratorBuilder(
    _periodMin = newPeriodMin,
    _periodMax = newPeriodMax,
    _probHi = this._probHi,
    _probLowJob = this._probLowJob,
    _maxERPoints = this._maxERPoints,
    _utilizationMin = this._utilizationMin,
    _utilizationMax = this._utilizationMax,
    _ratioHighWCETtoLowWCET = this._ratioHighWCETtoLowWCET
    )
}

