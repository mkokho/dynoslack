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

  private var globalTime: Int = 0

  /**
   * LoCriticalTasks will be changed at runtime.
   * We keep the change in this variable
   */
  private var taskState: mutable.Map[Task, Task] = mutable.HashMap.empty ++ tasks.map(x => x -> x)



  /**
   * Jobs that will be scheduled next.
   * @return a sequence of size $this.arity
   */
  override def next(): Seq[ScheduledJob] = ???
}
