package org.kokho.scheduling_new

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * This trait implements a proxy for job objects.
 * It forwards all calls to a different job object.
 */
trait JobProxy extends Job {

  def proxyJob: Job

  override def release: Int = proxyJob.release

  override def length: Int = proxyJob.length

  override def deadline: Int = proxyJob.deadline

  override def releasedBy: Option[Task] = proxyJob.releasedBy
}
