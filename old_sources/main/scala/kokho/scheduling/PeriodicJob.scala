package org.kokho.scheduling

/**
* Created with IntelliJ IDEA on 6/4/15.
* @author: Mikhail Kokho
*/

/**
 * Represents a job of a periodic task.
 *
 * A periodic task releases job at specific moments of time.
 * Therefore a job can be computed by its index.
 * A job of the same task with the same index are equal.
 *
 * @param idx - index of a job
 * @param task - task that produced this job
 */
case class PeriodicJob(idx: Int, task: Task) extends Job {
  def release = idx * task.period + task.offset

  def deadline = release + task.period

  def length = task.execution

  override def releasedBy = Option(task)
}
