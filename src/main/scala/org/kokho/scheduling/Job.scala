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

  def isOfTask(task: Task): Boolean = releasedBy match {
    case None => false
    case Some(otherTask) => task == otherTask
  }


  override def toString: String = releasedBy match {
    case None => s"Job($length in $release:$deadline)"
    case Some(task) =>
      task.name  match {
        case "NoName" => s"Job($length in $release:$deadline)"
        case name: String => s"$name($length in $release:$deadline)"
      }
  }
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