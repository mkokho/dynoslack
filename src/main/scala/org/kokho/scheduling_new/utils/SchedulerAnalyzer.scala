package org.kokho.scheduling_new.utils

import org.kokho.scheduling_new._
import org.kokho.scheduling_new.multicritical.system.LoCriticalTask

import scala.collection.immutable.HashSet
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA on 6/7/2015.
 * @author: Mikhail Kokho
 */
class SchedulerAnalyzer(val scheduler: Scheduler,
                       val memory: Int = 30,
                       val fixedMemory: Boolean = false) {

//  val schedule = scheduler.schedule(memory)

  lazy val taskToJobs = computeTaskToJobs()
  private val jobsStream = toJobSequences(scheduler, memory)

  def totalIdleTimeBefore(t: Int): Int = jobsStream.map(seq => totalIdleTime(seq.take(t))).sum

  def totalIdleTime: Int = jobsStream.map(totalIdleTime(_)).sum

  def totalIdleTime(seq: Seq[ScheduledJob]): Int = seq.toIterator.map({
    case ScheduledJob(_, _, IdleJob) => 1
    case _ => 0
  }).sum

  def taskFrequency(task: Task): Int = {
    require(scheduler.tasks.contains(task))
    taskToJobs(task).size
  }

  def numberOfEarlyReleases(loTask: LoCriticalTask) = {
    require(scheduler.tasks.contains(loTask))
    var offset = 0
    var count = 0
    for (job <- findJobs(loTask)) {
      val releasedBy = job.scheduledJob.releasedBy.get
      if (releasedBy.offset != offset){
        count += 1
        offset = releasedBy.offset
      }
    }

    count
  }

  def findJobs(task: Task): Seq[ScheduledJob] = {
    def fiterAndMerge(seq: Seq[ScheduledJob]) = mergeScheduledJobs(seq.filter(_.scheduledJob.isOfTask(task)))

    require(scheduler.tasks.contains(task), "There is no such task in the schedule")
    jobsStream.map(fiterAndMerge).flatten
  }

  def debugInfo(from: Int, length: Int): String = {
    def format(seq: Seq[ScheduledJob]): String =
      seq.drop(from).take(length).map(_.toString.padTo(31, " ").mkString).mkString

    jobsStream.map(format).mkString("\n")
  }

  def printSchedule(limit: Int = 20): Unit = {
    jobsStream map mergeScheduledJobs foreach println
  }

  private def mergeScheduledJobs(jobsFlow: Seq[ScheduledJob]): Seq[ScheduledJob] = {
    val reverseSchedule = jobsFlow.foldLeft(List[ScheduledJob]())(
      (acc, sJob) => acc match {
        case Nil => List(sJob)
        case head :: tail if sJob.isConsecutive(head) => head.merge(sJob) :: tail
        case _ => sJob :: acc
      }
    )

    reverseSchedule.reverse
  }

  def jobStream(idx: Int): Seq[ScheduledJob] = {
    require(idx < jobsStream.size)
    mergeScheduledJobs(jobsStream(idx))
  }

  def findDoubleReleases(): Option[Job] = {
    val allJobs: Seq[ScheduledJob] = jobsStream.flatten
      .filter(!_.isIdle)
      .sortWith(_.to < _.to)

    val taskToJobs = allJobs.groupBy(_.scheduledJob.releasedBy.get)
    var doubleRelease: Option[Job] = None
    for {
      (task, jobs) <- taskToJobs if doubleRelease.isEmpty
      (job, allocation) <- jobs.groupBy(_.scheduledJob)
      pair <- allocation.sliding(2) if pair.size > 1
    } {
      if (pair(0).to > pair(1).from) {
        doubleRelease = Some(job)
      }
    }

    doubleRelease
  }

  def findMigratedJobs(): Option[Job] = {
    val hashSets: Seq[HashSet[Job]] = jobsStream.map(seq =>
      HashSet() ++ seq.iterator.map(_.scheduledJob).filter(_ != IdleJob)
    )

    var migratedJob: Option[Job] = None
    for {
      set <- hashSets
      job <- set if migratedJob.isEmpty
      otherSet <- hashSets if set != otherSet
    } {
      if (otherSet.contains(job)) migratedJob = Some(job)
    }

    migratedJob
  }

  def findUncompletedJobs(): Seq[Seq[Job]] = jobsStream.map(findUncompletedJobs)

  private def findUncompletedJobs(jobs: Seq[ScheduledJob]) = {
    val sGrouped: Map[Job, Int] = jobs.groupBy(_.scheduledJob).mapValues(ts => ts.foldLeft(0)(_ + _.length))

    {
      for (
        j <- sGrouped.keys
        if j != IdleJob
        if j.length != sGrouped(j)
      ) yield j
    }.toSeq
  }

  def findOverdueJobs(): Seq[Seq[ScheduledJob]] = jobsStream.map(findOverdueJobs)

  def findUncompletedJobs(idx: Int): Seq[Job] = {
    require(idx < jobsStream.size)
    findUncompletedJobs(jobsStream(idx))
  }

  def findOverdueJobs(idx: Int): Seq[ScheduledJob] = {
    require(idx < jobsStream.size)
    findOverdueJobs(jobsStream(idx))
  }

  private def findOverdueJobs(jobs: Seq[ScheduledJob]) = jobs.filter(_.isOverdue)

  private def isOrdered(seq: Seq[ScheduledJob]) = {
    val res = seq.sliding(2).find(pair => pair(0).to > pair(1).from)
    res match {
      case None => true
      case Some(pair) => false
    }
  }

  private def computeTaskToJobs(): Map[Task, Set[Job]] = {
    val mapBuilder = mutable.Map.empty[Task, mutable.Set[Job]]
    for (task <- scheduler.tasks) mapBuilder.put(task, mutable.Set.empty[Job])

    for {
      job <- jobsStream.flatten.filter(!_.isIdle).map(_.scheduledJob)
      task <- scheduler.tasks
      if job.isOfTask(task)
    } {
      val setBuilder = mapBuilder.get(task).get
      setBuilder += job
    }

    mapBuilder.mapValues(_.toSet).toMap
  }

  /**
   * Schedule gives us jobs that are scheduled on each processor at time x.
   * This methods transforms a schedule to sequences that contains jobs for each processor
   */
  private def toJobSequences(scheduler: Scheduler, limit: Int): Seq[Seq[ScheduledJob]] = {
//    def helper(itr: Iterator[Seq[ScheduledJob]], acc: Seq[Seq[ScheduledJob]]): Seq[Seq[ScheduledJob]] ={
//      acc = acc + itr.next().transpose
//    }

    val res = scheduler.schedule(limit).toSeq.transpose
    if (fixedMemory || res.flatMap(findUncompletedJobs).isEmpty)
      res
    else {
      toJobSequences(scheduler, limit+1)
    }

  }
}
