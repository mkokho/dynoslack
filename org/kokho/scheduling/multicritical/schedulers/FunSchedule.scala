package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling._

import scala.collection.immutable.ListSet

/**
 * Class-helper for construction of schedulers.
 *
 * Its purpose is to maintain a set of active jobs and, given a job stream
 * and implicit ordering, choose what job will be scheduled.
 *
 * We make this class immutable to allow easy computations and predictions
 *
 * @author: Mikhail Kokho
 * @date: 6/25/15.
 */

class FunSchedule private(val time: Int,
                          val incompletedJobs: Set[ActiveJob],
                          val jobStream: JobStream)(implicit val priority: Ordering[Job]) {

  assert(incompletedJobs.forall(!_.isComplete), "There must not be completed jobs")
  assert(incompletedJobs.forall(_.job.deadline > time), s"There is an overdue job at time $time: ${incompletedJobs.find(_.job.deadline <= time)}")
  assert(incompletedJobs.forall(_.job.release <= time), "There must not be jobs are not released yet")

  val releasedJobs = jobStream.produceAt(time)
  val activeJobs = incompletedJobs ++ releasedJobs.map(ActiveJob(_, 0))
  val minJob = activeJobs.min.job
  val scheduledJob = ScheduledJob(time, time + 1, minJob)

  def slackStream(to: Int): Stream[SlackUnit] =
    if (time >= to) Stream.empty
    else minJob match {
      case IdleJob => SlackUnit(time) #:: nextState().slackStream(to)
      case _ => nextState().slackStream(to)
    }

  def nextState() = {
    def executeMinJob(aj: ActiveJob) = if (aj.job == minJob) aj.execute() else aj

    val nextIncompletedJobs = activeJobs.map(executeMinJob).filter(_.nonComplete)

    new FunSchedule(time + 1, nextIncompletedJobs, jobStream)
  }

  def update(js: JobStream) = new FunSchedule(time, incompletedJobs, js)

//  def availableSlack(before: Int) = slackStream.takeWhile(_.start < before).length

  def isActive(task: Task) = activeJobs.exists(_.job.isOfTask(task))


  /*

    def isActive(job: Job) = activeJobs.exists(_.job == job)


    def isBusy = activeJobs.nonEmpty

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
  */

  implicit def activeJobOrdering: Ordering[ActiveJob] = Ordering.by(_.job)

}

object FunSchedule {

  def apply(js: JobStream)(implicit priority: Ordering[Job]) = new FunSchedule(0, ListSet(ActiveJob(IdleJob, 0)), js)

}

/**
 * Represents a job in the schedule that is currently being active
 */
case class ActiveJob(job: Job, executedFor: Int = 0) {
  def execute() = {
    assert(!isComplete, "Trying to execute already completed job")
    ActiveJob(job, executedFor + 1)
  }

  def isComplete = job.length == executedFor

  def nonComplete = !isComplete

}