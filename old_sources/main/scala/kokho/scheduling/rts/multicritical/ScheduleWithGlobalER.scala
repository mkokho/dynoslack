package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduledJob

/**
 * Created with IntelliJ IDEA on 6/5/15.
 * @author: Mikhail Kokho
 */

/**
 * Support local early releases of the jobs
 */
final class ScheduleWithGlobalER(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition){

  private def releaseEarlyJobsGlobally() ={
    //tasks that can release an early jobs
    val tasksForER = localSchedules.map(_.tasksForEarlyRelease).flatten

    //we might want to order tasks
    //now we just use random fit
    for {
      task <- tasksForER
      localSchedule <- localSchedules find (_.hasSlackForTask(task))
      scheduleOfTask <- taskToLocalSchedule(task)
    }{
      val job = scheduleOfTask.releaseEarlyJob(task)
      localSchedule.insertJob(job)
    }

  }

  override def next(): Seq[ScheduledJob] = {
    releaseEarlyJobsGlobally()
    localSchedules.map(itr => itr.next()).toList
  }


}
