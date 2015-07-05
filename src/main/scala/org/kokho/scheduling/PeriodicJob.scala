package org.kokho.scheduling

/**
* Created with IntelliJ IDEA on 6/4/15.
* @author: Mikhail Kokho
*/

/**
 * Represents a job of a periodic task.
 *
 * A periodic job must be released by a periodic task.
 * We compute all parameters of a job from the release time and the task
 */
trait PeriodicJob extends Job {

  /**
   * A task that released this job
   */
  def task: PeriodicTask

  /**
   * Index of the job.
   */
  def index: Int = (release - task.offset) / task.period

  override def length: Int = task.execution

  override def deadline: Int = release + task.deadline

  override def releasedBy: Option[Task] = Some(task)
}
