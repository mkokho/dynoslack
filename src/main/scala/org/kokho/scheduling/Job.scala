package org.kokho.scheduling

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

  def releasedBy: Option[Task] = None

  override def toString: String = s"Job($length->$release:$deadline)"

}


object Job {

  def apply(release_ : Int, length_ : Int, deadline_ : Int) = new Job {
    override def release: Int = release_

    override def length: Int = length_

    override def deadline: Int = deadline_
  }

  def apply(job_ : Job) = new JobProxy {
    override def job: Job = job_
  }
}