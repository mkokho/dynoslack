package org.kokho.scheduling

import org.kokho.scheduling.multicritical.system.LoCriticalTask

import scala.collection.immutable.HashSet

/**
 * Utility class that provides methods for analysis of a scheduler
 *
 * @author: Mikhail Kokho
 * @date: 6/7/2015
 */
class SchedulerAnalyzer(val scheduler: Scheduler, val until: Int = 30) {


  lazy val taskToJobs = computeTaskToJobs()
  lazy val coreToJobs = toJobSequences(scheduler, until)

  def totalIdleTime: Int = coreToJobs.map(seqIdleTime).sum

  def seqIdleTime(seq: Seq[ScheduledJob]): Int = seq.count(_.isIdle)

/*  def taskFrequency(task: Task): Int = {
    require(scheduler.tasks.contains(task))
    taskToJobs(task).size
  }*/

  def countEarlyReleases(loTask: LoCriticalTask) = {
    require(scheduler.tasks.contains(loTask))
    def hasDifferentParents(seq:Seq[Job]) = seq match {
      case Seq(a,b) => a.releasedBy != b.releasedBy
    }

    val jobs = findJobs(loTask).map(_.scheduledJob)

    if (jobs.size <= 1) 0
    else jobs.sliding(2).count(hasDifferentParents)
  }

  def findJobs(task: Task): Seq[ScheduledJob] = {
    def fiterAndMerge(seq: Seq[ScheduledJob]) = mergeScheduledJobs(seq.filter(_.scheduledJob.isOfTask(task)))

    require(scheduler.tasks.contains(task), "There is no such task in the schedule")
    coreToJobs.map(fiterAndMerge).flatten.sortBy(_.from)
  }
  
  /**
   * Make an input sequence of scheduled jobs more compact by
   * merging consecutive jobs
   */
  def mergeScheduledJobs(jobsFlow: Seq[ScheduledJob]): Seq[ScheduledJob] = {
    val reverseSchedule = jobsFlow.foldLeft(List[ScheduledJob]())(
      (acc, sJob) => acc match {
        case Nil => List(sJob)
        case head :: tail if sJob.isConsecutive(head) => head.merge(sJob) :: tail
        case _ => sJob :: acc
      }
    )

    reverseSchedule.reverse
  }

  /**
   * A job is double released if it has been released before
   * the other job of the same task has been completed
   */
  def findDoubleReleases(): Option[Job] = {
    val jobToScheduled = coreToJobs
      .flatten
      .filter(_.scheduledJob.releasedBy.isDefined)
      .groupBy(_.scheduledJob)

    val sortedKeys = jobToScheduled.keysIterator.toList.sortWith(_.release < _.release)

    val doubleReleases = for {
      Seq(job, jobOther) <- sortedKeys.sliding(2)
      if job.releasedBy == jobOther.releasedBy
      jobLastExecution = jobToScheduled(job).last
      jobOtherFirstExecution = jobToScheduled(jobOther).head
      if jobOtherFirstExecution.isScheduledBefore(jobLastExecution)
    } yield jobOther

    doubleReleases.find(_ => true)
  }

  /**
   * A job is migrated if it has been scheduled on two processors at different times
   */
  def findMigratedJobs(): Option[Job] = {
    val hashSets: Seq[HashSet[Job]] = coreToJobs.map(seq =>
      HashSet() ++ seq.filter(!_.isIdle).map(_.scheduledJob)
    )

    //we search for intersections between every two sets
    val migratedJobs = for {
      Seq(set, otherSet) <- hashSets.combinations(2)
      job <- set.intersect(otherSet)
    } yield job

    migratedJobs.find(_ => true)
  }

  /**
   * A job is incorrectly scheduled if it has been executed on the processors 
   * for the time that is different ffrom the job's length.
   * A job is NOT incorrectly scheduled if its deadline is in the future
   */
  def findIncorrectlyScheduled(): Option[Job] = {
    val allJobs = coreToJobs.flatten.filter(!_.isIdle)

    val jobToExecution: Map[Job, Int] = allJobs
      .groupBy(_.scheduledJob)
      .mapValues(ts => ts.foldLeft(0)(_ + _.length))

    val uncompletedJob = jobToExecution.find{
      case (job, exec) => job.deadline <= until && job.length != exec
    }

    uncompletedJob.map(_._1)
  }

  def findOverdueJobs(): Seq[Seq[ScheduledJob]] = coreToJobs.map(findOverdueJobs)

  def findOverdueJobs(jobs: Seq[ScheduledJob]) = jobs.filter(_.isOverdue)


  private def computeTaskToJobs(): Map[Task, Set[Job]] = {
    val allJobs = coreToJobs
      .flatten
      .map(_.scheduledJob)
      .filter(_.releasedBy.isDefined)
      .toSet

    scheduler.tasks
      .map(task => task -> allJobs.filter(_.isOfTask(task)))
      .toMap

  }

  /**
   * Schedule gives us jobs that are scheduled on each processor at time x.
   * This methods transforms a schedule to sequences that contains jobs for each processor
   */
  private def toJobSequences(scheduler: Scheduler, limit: Int): Seq[Seq[ScheduledJob]] = {
    scheduler.schedule(limit).toSeq.transpose
  }
}
