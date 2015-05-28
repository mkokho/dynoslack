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
  def apply(release_ : Int, length_ : Int, deadline_ : Int) = new Job {

    override def length: Int = length_

    override def deadline: Int = deadline_

    override def release: Int = release_
  }
}

abstract class JobDecorator(job: Job) extends Job{
  override def release: Int = job.release

  override def length: Int = job.length

  override def deadline: Int = job.deadline
}
