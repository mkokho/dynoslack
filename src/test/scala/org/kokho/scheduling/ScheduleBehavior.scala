package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created by Mikhail Kokho on 6/1/2015.
 */
trait ScheduleBehavior {
  this : FlatSpec =>

  private def takeWhileBusy(schedule: Schedule): List[Seq[ScheduledJob]] =
    if (schedule.isBusy)
      schedule.next() :: takeWhileBusy(schedule)
    else
      Nil

  def jobStreams(schedule: Schedule): Seq[Seq[ScheduledJob]] = {
    //we save output of the schedule
    val limit = 1000
    //first we take a fixed number of elements
    val scheduleFixedLength = schedule.take(limit).toList.toSeq
    //then we take elements while schedule remains busy
    //@TODO for some cases schedule may remain busy forever
    //it is a custom method because takeWhile advances iterator, and then checks the schedule
    //therefore we miss last element
    val scheduleWhileBusy = takeWhileBusy(schedule)

    val savedJobs = scheduleFixedLength ++ scheduleWhileBusy
    for {
      idx <- 0.until(schedule.arity)
    } yield savedJobs collect { case coreToJob => coreToJob(idx)}
  }

  private def findUncompletedJob(jobs: Seq[ScheduledJob]) = {
      val sGrouped:Map[Job, Int] = jobs.groupBy(_.job).mapValues(ts => ts.foldLeft(0)(_ + _.length))

      for (
        j <- sGrouped.keys
        if j != IdleJob
        if j.length != sGrouped(j)
      ) yield j
    }

    private def findOverdueJob(jobs: Seq[ScheduledJob]) = {
      jobs.filter(scheduledJob => scheduledJob.to > scheduledJob.job.deadline)
    }

    def aSchedule(schedule: Schedule): Unit = {
      val coreJobs = jobStreams(schedule)

      it should "not contain uncompleted jobs" in {
        coreJobs foreach {jobs =>
          val uncompletedJobs = findUncompletedJob(jobs)
          assert(uncompletedJobs.size == 0, s"Uncompleted job: $uncompletedJobs")
        }
      }

      it should "not contain overdue jobs" in {
        coreJobs foreach {jobs =>
          val overdueJobs = findOverdueJob(jobs)
          assert(overdueJobs.size == 0, s"Uncompleted job: $overdueJobs")
        }
      }

      it should "contain at least one job of each task" in {

      }

    }

}
