package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.ScheduledJob
import org.kokho.scheduling.multicritical.system.MulticriticalTask

/**
 * Scheduler that support global release of low-criticality tasks
 *
 * @author: Mikhail Kokho
 * @date 7/3/15.
 */
class SchedulerWithGlobalER(override val partition: Seq[Seq[MulticriticalTask]]) extends MulticriticalScheduler(partition){
  self =>
  /**
   * Infinite iterator over a sequence of scheduled jobs
   */
  override def iterate(): Iterator[Seq[ScheduledJob]] = new Iterator[Seq[ScheduledJob]]{
    override def hasNext: Boolean = true

    val schedulingWorkers = self.partition.map(new MulticriticalWorker(_))

    private def releaseGlobally() = {
      for {
        hostWorker <- schedulingWorkers
        guestWorker <- schedulingWorkers if guestWorker != hostWorker
        loTask <- hostWorker.tasksForEarlyRelease
        if schedulingWorkers.forall(_.notActive(loTask))
      } {
        val deadline = hostWorker.currentTime + loTask.period
        val demand = loTask.execution

        if (guestWorker.slackUntil(deadline) >= demand){
          val earlyJob = hostWorker.extractEarlyJob(loTask)
          guestWorker.insertEarlyJob(earlyJob)
        }
      }
    }



    override def next(): Seq[ScheduledJob] = {
      schedulingWorkers.foreach(_.releaseLocally())
      releaseGlobally()
      val jobs = schedulingWorkers.map(_.next())
      jobs
    }
  }
}
