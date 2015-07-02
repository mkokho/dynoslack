package org.kokho.scheduling_new

import org.kokho.binpacking.{BinPacker, WeightedObject}
import org.kokho.scheduling.exceptions.UnschedulableSetException

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 * Simulates execution of tasks on some processors
 *
 */
trait Scheduler {

  type AllowedTasks <: Task

  /**
   * Tasks that are being scheduled
   */
  def tasks: Seq[AllowedTasks]

  /**
   * The number of processors used by the scheduler
   */
  def arity: Int

  /**
   * Jobs that will be scheduled next.
   * @return a sequence of size $this.arity
   */
  def next(): Seq[ScheduledJob]


  /**
   * Iterator over the scheduled jobs
   */
  def schedule(): Iterator[Seq[ScheduledJob]] = Iterator.continually(next())

}

