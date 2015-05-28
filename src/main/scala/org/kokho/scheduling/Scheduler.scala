package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 *  Computes a schedule for a set of tasks, or throws UnschedulableSetException
 *
 *
 */
trait Scheduler {

  type MulticoreSchedule = Iterator[Core => Job]

  def tasks:Set[Task]

  def cores: Seq[Core]

  def isSchedulable: Boolean

  def schedule():MulticoreSchedule

}

case class ScheduledJob(from: Int, to: Int, job: Job) {
  require(to > from, s"Incorrect interval. The end is smaller than the beginning: [$from, $to]. ")

  def length = to - from

  override def toString: String = job + "->" + from + ":" + to
}