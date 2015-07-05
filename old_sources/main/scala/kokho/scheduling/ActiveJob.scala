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
//  private var remainingTime = job.length
  private var _elapsedTime = 0

  def length = job.length

  def elapsedTime = _elapsedTime

//  def deadline = job.deadline

//  def isCompleted = remainingTime == 0
  def isCompleted = _elapsedTime >= job.length

//  def isBusy = !isCompleted && remainingTime < job.length
  def isBusy = !isCompleted && _elapsedTime > 0

  def execute(): ActiveJob = {
//    if (isCompleted)
//      throw new IllegalStateException(s"The job $job has been completed. Cannot execute it")

    val that = ActiveJob(job)
    that._elapsedTime = _elapsedTime + 1
    that
  }
}