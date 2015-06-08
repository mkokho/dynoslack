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
  lazy val tasks: Seq[Task] = partition.flatten

  /**
   * Hyper-period of the tasks of the set partition($idx)
   */
  def hyperPeriod(idx: Int): Int = {
    if (idx < 0 || idx >= arity) {
      throw new IllegalArgumentException(
        s"Index is out of bound. Expected between 0 and $arity. Given $idx"
      )
    }

    org.kokho.utils.Math.lcm(partition(idx).map(_.period).iterator)
  }
}
