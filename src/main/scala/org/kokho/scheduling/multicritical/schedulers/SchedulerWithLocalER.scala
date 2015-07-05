package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.ScheduledJob
import org.kokho.scheduling.multicritical.system.MulticriticalTask


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

    override def hasNext: Boolean = true

    val schedulingWorkers = self.partition.map(new MulticriticalWorker(_))

    override def next(): Seq[ScheduledJob] = {
      schedulingWorkers.foreach(_.releaseLocally())
      val jobs = schedulingWorkers.map(_.next())
      jobs
    }

  }

}
