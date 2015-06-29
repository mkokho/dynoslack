package org.kokho.scheduling_new

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 * Represents a piece of work that must be completed in the period
 * from $release to $deadline and requires $length unit of times.
 *
 */
trait Job {
  def release: Int

  def length: Int

  def deadline: Int

  def relativeDeadline = deadline - release

  def releasedBy: Option[Task] = None

  override def toString: String = releasedBy match {
    case None => s"Job($length in $release:$deadline)"
    case Some(task) => s"${task.name}($length in $release:$deadline)"
  }
}


object Job {

  def apply(release_ : Int, length_ : Int, deadline_ : Int) = new Job {
    override def release: Int = release_

    override def length: Int = length_

    override def deadline: Int = deadline_
  }

  def apply(job_ : Job) = new JobProxy {
    override def proxyJob: Job = job_
  }
}


/**
 * When there are no active jobs to execute, processor is idle.
 * IdleJob represents idle time on a processor
 */
object IdleJob extends Job {
  override def release: Int = 0

  override def length: Int = Integer.MAX_VALUE

  override def deadline: Int = Integer.MAX_VALUE

  override def toString: String = "IJ"

}