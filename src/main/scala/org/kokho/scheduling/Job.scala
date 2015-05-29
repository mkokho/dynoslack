package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait Job {

  def release: Int

  def length: Int

  def deadline: Int

  override def toString: String = s"Job($length->$release:$deadline)"

}

trait ForwardingJob extends Job {

  def job: Job

  override def release: Int = job.release

  override def length: Int = job.length

  override def deadline: Int = job.deadline
}

object Job {
  /**
   * Defines a default constructor of Job objects
   */
  final class DefaultJobConstructor(val release:Int, val length:Int, val deadline: Int) extends Job {}

  def apply(release: Int, length: Int, deadline: Int) = new DefaultJobConstructor(release, length, deadline)

  def apply(job_ : Job) = new ForwardingJob {
    override def job: Job = job_
  }
}