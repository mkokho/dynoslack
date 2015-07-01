package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling.rts.multicritical.HiCriticalTask
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

  /**
   * LoCriticalTasks will be changed at runtime.
   * We keep the change in this variable
   */
  private val taskState: mutable.Map[Task, Task] = mutable.HashMap.empty ++ tasks.map(x => x -> x)

  private val schedules: mutable.Seq[SchedulerHelper] = mutable.Seq.empty ++ partition.map(seq => SchedulerHelper(toJobStream(seq)))

  /**
   * Jobs that will be scheduled next.
   * @return a sequence of size $this.arity
   */
  override def next(): Seq[ScheduledJob] = {
    val result = schedules.map(_.scheduledJob)

    val nextTime = currentTime + 1
    for {
      idx <- 0 to partition.size
      loTask <- partition(idx) collect {case t: LoCriticalTask => t}
      if loTask.canReleaseEarlyJob(nextTime)

    }

    result
  }

  private def currentTime = schedules(0).time
}
