package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * A job that has been released but has not been completed.
 * For use in schedules.
 *
 * @param job a job that awaits its turn for execution on a processor
 */
case class ActiveJob(job: Job) {
  private var remainingTime = job.length

  def length = job.length

//  def deadline = job.deadline

  def isCompleted = remainingTime == 0

  def isBusy = !isCompleted && remainingTime < job.length

  def execute(): ActiveJob = {
    if (isCompleted)
      throw new IllegalStateException(s"The job $job has been completed. Cannot execute it")

    val that = ActiveJob(job)
    that.remainingTime = remainingTime - 1
    that
  }
}
