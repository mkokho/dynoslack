package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * Represents execution of a job
 *
 * @param from start of execution
 * @param to end of execution
 * @param job scheduled job
 */
case class ScheduledJob(from: Int, to: Int, job: Job) {
  require(to > from, s"Incorrect interval. The end is smaller than the beginning: [$from, $to]. ")

  def length = to - from

  def extend() = new ScheduledJob(from, to+1, job)
  
  def merge(that: ScheduledJob) = {
    require(that.job == this.job, "To be merged scheduled jobs must have the same underlying jobs")
    require(this.to == that.from || this.from == that.to, "To be merged scheduled jobs must be consecutive")
    new ScheduledJob(Math.min(this.from, that.from), Math.max(that.to, this.to), job)
  }
  
  def isConsecutive(that: ScheduledJob) =
    this.job == that.job && (this.to == that.from || that.to == this.from)

  def isOfTask(task: Task): Boolean = this.job.isOfTask(task)
//    releasedBy match {
//    case None => false
//    case Some(otherTask) => task == otherTask
//  }


  override def toString: String = job + "->" + from + ":" + to
}
