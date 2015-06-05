package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduledJob

/**
 * Created with IntelliJ IDEA on 6/5/15.
 * @author: Mikhail Kokho
 */

/**
 * Support local early releases of the jobs
 */
final class ScheduleWithLocalER(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition){

  private def releaseEarlyJobsLocally() = {
    for {
      lSch <- localSchedules
      task <- lSch.tasksForEarlyRelease
      if lSch.hasSlackForTask(task)
    } {
      val job = lSch.releaseEarlyJob(task)
      lSch.insertJob(job)
    }
  }

  override def next(): Seq[ScheduledJob] = {
    releaseEarlyJobsLocally()
    localSchedules.map(itr => itr.next())
  }


}
