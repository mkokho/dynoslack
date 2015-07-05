package org.kokho.scheduling.multicritical.system


/**
 * A task that release low critical jobs.
 *
 * A task is an immutable object. But a low critical task is mutable by definition - once it releases
 * an early jobs, all future jobs are released relatively to the release of the last early job.
 * To overcome this problem, instead of mutating a low critical task object, we create another object
 * that has the same parameters as the parent object, but can only release jobs after the moment of time
 * it was created.
 *
 * @author: Mikhail Kokho
 * @date:  5/28/15
 */
trait LoCriticalTask extends MulticriticalTask {
  self =>

  override type JobType = LoCriticalJob

  def earlyReleases: List[Int]

  /**
   * Returns a new LoCritical tasks that releases jobs starting from $time
   *
   * @param time the moment of time when the early jobs will be released.
   * @return a new LoCriticalTasks that shifts releases of future jobs
   */
  def shift(time: Int): LoCriticalTask

  override def buildJob(release: Int): LoCriticalJob = new LoCriticalJob(release, this)

  /**
   * Given absolute time, calculates the time relative to the period.
   */
  def toRelativeTime(time: Int) = (time - offset) % period

  def canReleaseEarlyJob(time: Int): Boolean = earlyReleases.contains(toRelativeTime(time))

  /**
   * Returns how much slack is needed to satisfy early release of a job
   */
  def demand(time: Int): Int = {
    val er = toRelativeTime(time)
    execution - (execution * er ) / period
  }

}


object LoCriticalTask {

  def apply(name_ : String, period: Int, loExecution: Int, earlyReleases: List[Int]): LoCriticalTask =
    new LoCriticalTaskDefault(period, loExecution, earlyReleases)
      with NamedTask {
      override def name: String = name_
    }

  def apply(period: Int, loExecution: Int): LoCriticalTask = apply(period, loExecution, loExecution.until(period).toList)

  def apply(period: Int, loExecution: Int, earlyReleases: List[Int]): LoCriticalTask = new LoCriticalTaskDefault(period, loExecution, earlyReleases)

}








