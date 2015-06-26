package org.kokho.scheduling.newmodel

import org.kokho.scheduling.rts.multicritical.{MulticriticalSchedule, MulticriticalTask}

/**
 * Created with IntelliJ IDEA on 6/26/15.
 * @author: Mikhail Kokho
 */
abstract class NewScheduleWithLocalER(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition)
  with EdfOrdering {

  private def releaseEarlyJobsLocally(): Unit = {

  }

  /*
    private var schedules = for {
      taskset <- partition
      jobSeq <- JobSequence(taskset)
    } yield new ScheduleFun(jobSeq)

  */
  /*
    def releaseEarlyJobsLocally() = {

    }

    override def next(): Seq[ScheduledJob] = {

    }*/

}
