package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.{Task, ScheduledJob}
import org.kokho.scheduling_new.multicritical.system.{LoCriticalTask, MulticriticalTask}

import scala.collection.mutable


/**
 * Scheduler that supports local early release of tasks
 *
 * @author: Mikhail Kokho
 * @date 6/29/15.
 */
class SchedulerWithLocalER(override val partition: Seq[Seq[MulticriticalTask]]) extends MulticriticalScheduler(partition){

  private val schedules = partition.map(new MulticriticalSchedule(_))

  /**
   * Jobs that will be scheduled next.
   * @return a sequence of size $this.arity
   */
  override def next(): Seq[ScheduledJob] = {
    releaseLocally()
    schedules.map(_.next())
  }

  private def releaseLocally() = {
    for {
      sch <- schedules
      loTask <- sch.tasksForEarlyRelease
      if sch.isLocalPossible(loTask)
    } {
      sch.releaseEarlyJob(loTask)
    }
  }

}
