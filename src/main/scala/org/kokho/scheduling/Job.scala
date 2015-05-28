package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait Job {

  def release:Int
  def length:Int
  def deadline:Int

  override def toString: String = s"Job($length->$release:$deadline)"

}

object Job {
  def apply(release: Int, length: Int, deadline: Int) = new Job {

    override def length: Int = length

    override def deadline: Int = deadline

    override def release: Int = release
  }
}

abstract class JobDecorator(job: Job) extends Job{
  override def release: Int = job.release

  override def length: Int = job.length

  override def deadline: Int = job.deadline
}
