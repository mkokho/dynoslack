package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * This trait implements a proxy for job objects.
 * It forwards all calls to a different job object.
 */
trait JobProxy extends Job {

  def job: Job

  override def release: Int = job.release

  override def length: Int = job.length

  override def deadline: Int = job.deadline

  override def releasedBy: Option[Task] = job.releasedBy
}
