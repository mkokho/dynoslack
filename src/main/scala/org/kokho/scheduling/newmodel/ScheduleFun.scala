package org.kokho.scheduling.newmodel

import org.kokho.scheduling.{IdleJob, Job, ScheduledJob, Task}

import scala.collection.immutable.ListSet

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */

class ScheduleFun(val jobs: JobSequence)(implicit val priority: Ordering[Job]) {

  private var globalTime = 0

  private var activeJobs: Set[ScheduleFun#ActiveJob] = ListSet.empty

  implicit def activeJobOrdering: Ordering[ScheduleFun#ActiveJob] = Ordering.by(_.job)

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
    val that = new ScheduleFun(newSeq)(priority)
    that.globalTime = this.globalTime
    that.activeJobs = this.activeJobs
    that
  }

  def execute(): ScheduledJob = {
    releaseJobs()
    globalTime = globalTime + 1

    ScheduledJob(globalTime - 1, globalTime, executeJob())
  }

  private def releaseJobs() = activeJobs ++ jobs.produceAt(globalTime).map(ActiveJob(_))

  private def executeJob(): Job =
    if (activeJobs.isEmpty) IdleJob
    else {
      val jobExec = activeJobs.min
      val newJob = jobExec.execute()

      activeJobs = activeJobs - jobExec
      if (!newJob.isComplete)
        activeJobs = activeJobs + newJob

      jobExec.job
    }


  /**
   * Represents a job in the schedule that is currently being active
   */
  protected case class ActiveJob(job: Job, executedFor: Int = 0) {
    assert(job.release <= globalTime & globalTime < job.deadline, s"The job $job is not active at time $globalTime")

    def execute() = {
      assert(!isComplete, "Trying to execute already completed job")
      ActiveJob(job, executedFor + 1)
    }

    def isComplete = job.length == executedFor

  }


}
