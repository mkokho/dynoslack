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

  override def toString: String = job + "->" + from + ":" + to
}
