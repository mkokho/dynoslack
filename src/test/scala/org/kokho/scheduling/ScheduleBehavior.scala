package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created by Mikhail Kokho on 6/1/2015.
 */
trait ScheduleBehavior {
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

    def aSchedule(schedule: Schedule): Unit = {
      val streamJobSeq = schedule.toStream

      val jobStreams:Seq[Stream[ScheduledJob]] =
        for {
          idx <- 0.until(schedule.arity)
          upto = schedule.hyperPeriod(idx)
        } yield streamJobSeq take upto collect {case coreToJob => coreToJob(idx)}


      it should "not contain uncompleted jobs" in {
        jobStreams foreach {jobs =>
          val uncompletedJobs = findUncompletedJob(jobs)
          assert(uncompletedJobs.size == 0, s"Uncompleted job: $uncompletedJobs.head")
        }
      }

      it should "not contain overdue jobs" in {
        jobStreams foreach {jobs =>
          val overdueJobs = findOverdueJob(jobs)
          assert(overdueJobs.size == 0, s"Uncompleted job: $overdueJobs.head")
        }
      }

      it should "contain at least one job of each task" in {

      }

    }

}
