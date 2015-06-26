package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * A schedule of execution of jobs produces by the tasks.
 */
abstract class Schedule extends Iterator[Seq[ScheduledJob]] {
  override def hasNext: Boolean = true

  /**
   * Each sequence of tasks in the partition is executed on its own processor
   */
  def partition: Seq[Seq[Task]]

  /**
   * True if there is an unfinished job on one of the processors
   */
  def isBusy: Boolean

  /**
   * Size of the tuples contained by this Schedule
   */
  def arity: Int = partition.size

  /**
   * The tasks of the schedule
   */
  def tasks: Seq[Task] = partition.flatten

  /**
   * Returns the index of the set that contains given task in the partition
   */
  def scheduleIndex(task: Task): Option[Int] = partition.indexWhere(_.contains(task)) match {
    case -1 => None
    case p => Some(p)
  }

}
