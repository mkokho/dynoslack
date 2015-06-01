package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created by Mikhail Kokho on 6/1/2015.
 */
trait ScheduleSpec {
  this : FlatSpec =>

    private def findUncompletedJob(jobs: Stream[ScheduledJob]) = {
      val sGrouped:Map[Job, Int] = jobs.groupBy(_.job).mapValues(ts => ts.foldLeft(0)(_ + _.length))

      for (
        j <- sGrouped.keys
        if j != IdleJob
        if j.length != sGrouped(j)
      ) yield j
    }

    private def findOverdueJob(jobs: Stream[ScheduledJob]) = {
      jobs.filter(scheduledJob => scheduledJob.to > scheduledJob.job.deadline)
    }


    def aValidSchedule(schedule: Schedule): Unit = {
      val savedJobs = schedule.toStream
      for (idx <- 0.until(schedule.arity)){
        val upto = schedule.hyperPeriod(idx)
        val jobs = savedJobs take upto collect {case coreToJob => coreToJob(idx)}

        assert(findUncompletedJob(jobs).size == 0)
        assert(findOverdueJob(jobs).size == 0)
      }

    }

}
