package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling.newmodel.JobSequence
import org.kokho.scheduling.{IdleJob, Job, ScheduledJob, Task}
import org.kokho.scheduling_new.JobStream

import scala.collection.immutable.ListSet

/**
 * Class-helper for construction of schedulers.
 *
 * Its purpose is to maintain a set of active jobs and, given a job stream
 * and implicit ordering, choose what job will be scheduled.
 *
 *
 *
 * @author: Mikhail Kokho
 * @date: 6/25/15.
 */

case class SchedulerHelper(time: Int, waitingJobs: Set[ActiveJob] = ListSet.empty)(implicit val priority: Ordering[Job]) {


  def schedule(jobStream: JobStream) = {
    val activeJobs = waitingJobs ++ jobStream.produceAt(time)


  }
  
  
/*
  private def releaseJobs() = activeJobs ++ jobs.produceAt(globalTime).map(ActiveJob(_))






  private var globalTime = 0

  private var activeJobs: Set[SchedulerHelper#ActiveJob] = ListSet.empty

  implicit def activeJobOrdering: Ordering[SchedulerHelper#ActiveJob] = Ordering.by(_.job)

  def isActive(job: Job) = activeJobs.exists(_.job == job)

  def isActive(task: Task) = activeJobs.exists(_.job.isOfTask(task))

  def isBusy = activeJobs.nonEmpty

  def time = globalTime


  def simulate(duration: Int): Seq[ScheduledJob] = {
    require(duration > 0, s"Parameter must be positive. Actual: $duration")
    val simulator = updated(jobs)

    for (_ <- 1 to duration)
    yield simulator.execute()
  }

  def updated(newSeq: JobSequence) = {
    val that = new SchedulerHelper(newSeq)(priority)
    that.globalTime = this.globalTime
    that.activeJobs = this.activeJobs
    that
  }

  def execute(): ScheduledJob = {
    releaseJobs()
    globalTime = globalTime + 1

    ScheduledJob(globalTime - 1, globalTime, executeJob())
  }


  private def executeJob(): Job =
    if (activeJobs.isEmpty) IdleJob
    else {
      val jobExec = activeJobs.min
      val newJob = jobExec.execute()

      activeJobs = activeJobs - jobExec
      if (!newJob.isComplete)
        activeJobs = activeJobs + newJob

      jobExec.job
    }*/
}


/**
 * Represents a job in the schedule that is currently being active
 */
protected case class ActiveJob(job: Job, executedFor: Int = 0) {
  def execute() = {
    assert(!isComplete, "Trying to execute already completed job")
    ActiveJob(job, executedFor + 1)
  }

  def isComplete = job.length == executedFor

}