package org.kokho.scheduling

import org.kokho.scheduling.rts.multicritical.LoCriticalTask

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA on 6/7/2015.
 * @author: Mikhail Kokho
 */
class ScheduleAnalyzer(val schedule: Schedule,
                       val memory: Int = 30,
                       val fixedMemory: Boolean = false) {
  private val jobsStream = toJobSequences(schedule, memory)

  lazy val taskToJobs = computeTaskToJobs()

  def totalIdleTimeBefore(t: Int): Int = jobsStream.map(seq => totalIdleTime(seq.take(t))).sum

  def totalIdleTime: Int = jobsStream.map(totalIdleTime(_)).sum

  def totalIdleTime(seq: Seq[ScheduledJob]): Int = seq.toIterator.map({
    case ScheduledJob(_, _, IdleJob) => 1
    case _ => 0
  }).sum

  def taskFrequency(task: Task): Int = {
    require(schedule.tasks.contains(task))
    taskToJobs(task).size
  }

  def numberOfEarlyReleases(loTask: LoCriticalTask) = {
    require(schedule.tasks.contains(loTask))
    var offset = 0
    var count = 0
    for (job <- findJobs(loTask)) {
      val releasedBy = job.job.releasedBy.get
      if (releasedBy.offset != offset){
        count += 1
        offset = releasedBy.offset
      }
    }

    count
  }


  def debugInfo(from: Int, length: Int): String = {
    def format(seq: Seq[ScheduledJob]): String =
      seq.drop(from).take(length).map(_.toString.padTo(31, " ").mkString).mkString

    jobsStream.map(format).mkString("\n")
  }

  def printSchedule(limit: Int = 20): Unit = {
    jobsStream map mergeScheduledJobs foreach println
  }

  def jobStream(idx: Int): Seq[ScheduledJob] = {
    require(idx < jobsStream.size)
    mergeScheduledJobs(jobsStream(idx))
  }

  def findJobs(task: Task): Seq[ScheduledJob] = {
    def fiterAndMerge(seq: Seq[ScheduledJob]) = mergeScheduledJobs(seq.filter(_.isOfTask(task)))

    require(schedule.tasks.contains(task), "There is no such task in the schedule")
    jobsStream.map(fiterAndMerge).flatten
  }

  def findDoubleReleases(): Option[Job] = {
    val allJobs: Seq[ScheduledJob] = jobsStream.flatten
      .filter(_.job != IdleJob)
      .sortWith(_.to < _.to)

    val taskToJobs = allJobs.groupBy(_.job.releasedBy.get)
    var doubleRelease: Option[Job] = None
    for {
      (task, jobs) <- taskToJobs if doubleRelease.isEmpty
      (job, allocation) <- jobs.groupBy(_.job)
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
      HashSet() ++ seq.iterator.map(_.job).filter(_ != IdleJob)
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

  def findOverdueJobs(): Seq[Seq[ScheduledJob]] = jobsStream.map(findOverdueJobs)

  def findUncompletedJobs(idx: Int): Seq[Job] = {
    require(idx < jobsStream.size)
    findUncompletedJobs(jobsStream(idx))
  }

  def findOverdueJobs(idx: Int): Seq[ScheduledJob] = {
    require(idx < jobsStream.size)
    findOverdueJobs(jobsStream(idx))
  }

  private def findOverdueJobs(jobs: Seq[ScheduledJob]) = {
    jobs.filter(scheduledJob => scheduledJob.to > scheduledJob.job.deadline)
  }

  private def findUncompletedJobs(jobs: Seq[ScheduledJob]) = {
    val sGrouped: Map[Job, Int] = jobs.groupBy(_.job).mapValues(ts => ts.foldLeft(0)(_ + _.length))

    {
      for (
        j <- sGrouped.keys
        if j != IdleJob
        if j.length != sGrouped(j)
      ) yield j
    }.toSeq
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

  private def isOrdered(seq: Seq[ScheduledJob]) = {
    val res = seq.sliding(2).find(pair => pair(0).to > pair(1).from)
    res match {
      case None => true
      case Some(pair) => false
    }
  }

  private def computeTaskToJobs(): Map[Task, Set[Job]] = {
    val mapBuilder = mutable.Map.empty[Task, mutable.Set[Job]]
    for (task <- schedule.tasks) mapBuilder.put(task, mutable.Set.empty[Job])

    for {
      job <- jobsStream.flatten.filter(!_.isIdle).map(_.job)
      task <- schedule.tasks
      if job.isOfTask(task)
    } {
      val setBuilder = mapBuilder.get(task).get
      setBuilder += job
    }

    mapBuilder.mapValues(_.toSet).toMap
  }

  /**
   * saves output of the schedule
   */
  private def toJobSequences(schedule: Schedule, limit: Int): Seq[Seq[ScheduledJob]] = {
    def takeWhileBusy(schedule: Schedule): List[Seq[ScheduledJob]] =
      if (!fixedMemory && schedule.isBusy)
        schedule.next() :: takeWhileBusy(schedule)
      else
        Nil

    //first we take a fixed number of elements
    //then we take elements while schedule remains busy
    //@TODO for some cases schedule may remain busy forever
    //it is a custom method because takeWhile advances iterator, and then checks the schedule
    //therefore we miss last element
    val builder = ArrayBuffer[Seq[ScheduledJob]]()
    builder ++= schedule.take(limit).toList
    builder ++= takeWhileBusy(schedule)

    val streams = Seq.fill(schedule.arity)(ArrayBuffer[ScheduledJob]())
    for {
      seq <- builder
      (flow, job) <- streams.zip(seq)
    } flow.append(job)

    assert(streams.forall(isOrdered))
    assert(streams(0).size >= memory)

    streams
  }
}
