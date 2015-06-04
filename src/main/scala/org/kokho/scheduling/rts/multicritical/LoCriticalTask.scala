package org.kokho.scheduling.rts.multicritical


/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 * A task that release low critical jobs.
 *
 * A task is an immutable object. But a low critical task is mutable by definition - once it releases
 * an early jobs, all future jobs are released relatively to the release of the last early job.
 * To overcome this problem, instead of mutating a low critical task object, we create another object
 * that has the smae parameters as the parent object, but can only release jobs after the moment of time
 * it was created.
 *
 */
trait LoCriticalTask extends MulticriticalTask {

  override type JobType = LoCriticalJob

  def earlyReleases: List[Int]

  /**
   * Returns a new LoCritical tasks that releases jobs starting from $time
   *
   * @param time the moment of time when the early jobs will be released.
   * @return a new LoCriticalTasks that shifts releases of future jobs
   */
  def shiftedTasks(time: Int): LoCriticalTask

  def canReleaseEarlyJob(time: Int): Boolean = {
    //calculate the early release time relative to the period
    val relativeRelease = (time - offset) % period

    earlyReleases.contains(relativeRelease)
  }
}


object LoCriticalTask {

  def apply(period: Int, loExecution: Int): LoCriticalTask = apply(period, loExecution, loExecution.until(period).toList)

  def apply(period: Int, loExecution: Int, earlyReleases: List[Int]): LoCriticalTask = new LoCriticalTaskParent(period, loExecution, earlyReleases)

}








