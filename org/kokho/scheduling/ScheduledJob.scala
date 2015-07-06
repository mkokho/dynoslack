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
 * @param scheduledJob scheduled job
 */
case class ScheduledJob(from: Int, to: Int, scheduledJob: Job) {

  require(to > from, s"Incorrect interval. The end is smaller than the beginning: [$from, $to]. ")

  def length = to - from

  def merge(that: ScheduledJob) = {
    require(that.scheduledJob == this.scheduledJob, "To be merged scheduled jobs must have the same underlying jobs")
    require(this.to == that.from || this.from == that.to, "To be merged scheduled jobs must be consecutive")
    new ScheduledJob(Math.min(this.from, that.from), Math.max(that.to, this.to), scheduledJob)
  }
  
  def isConsecutive(that: ScheduledJob) =
    this.scheduledJob == that.scheduledJob && (this.to == that.from || that.to == this.from)

  def isIdle = scheduledJob == IdleJob

  def isOverdue = to > scheduledJob.deadline

  def isScheduledBefore(that: ScheduledJob) = this.from <= that.to


  override def toString: String = scheduledJob + "->" + from + ":" + to
}
