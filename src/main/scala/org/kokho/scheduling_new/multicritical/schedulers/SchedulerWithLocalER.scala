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
  self =>

  /**
   * Jobs that will be scheduled next.
   * @return a sequence of size $this.arity
   */
  override def iterate() = new Iterator[Seq[ScheduledJob]]{

    val schedules = self.partition.map(new MulticriticalWorker(_))

    override def hasNext: Boolean = true

    override def next(): Seq[ScheduledJob] = {
      releaseLocally(schedules)
      val jobs = schedules.map(_.next())
      jobs
    }

  }

  private def releaseLocally(schedules: Seq[MulticriticalWorker]) = {
    for {
      sch <- schedules
      loTask <- sch.tasksForEarlyRelease
      if sch.isLocalPossible(loTask)
    } {
      sch.releaseEarlyJob(loTask)
    }
  }


}
