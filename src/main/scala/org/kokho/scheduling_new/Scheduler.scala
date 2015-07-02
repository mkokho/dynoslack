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
  scheduler =>

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
   * Infinite iterator over a sequence of scheduled jobs
   */
  def iterate(): Iterator[Seq[ScheduledJob]]


  /**
   * Iterator over the scheduled jobs
   */
  def schedule(p: Seq[ScheduledJob] => Boolean): Schedule = new Schedule {

    override def iterator: Iterator[Seq[ScheduledJob]] = iterate().takeWhile(p).toList.iterator

  }

  def schedule(until: Int): Schedule = this.schedule(seq => seq(0).to <= until)

}

