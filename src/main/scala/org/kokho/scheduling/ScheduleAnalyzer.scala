package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/7/2015.
 * @author: Mikhail Kokho
  */
class ScheduleAnalyzer(val schedule: Schedule, val memory: Int = 30) {
  private val jobsStream = toJobSequences(schedule, memory)

  def extend(until: Int):ScheduleAnalyzer = if (until < memory) this else new ScheduleAnalyzer(schedule, until)

  def findJobs(task: Task): Seq[ScheduledJob] = {
    require(schedule.tasks.contains(task), "There is no such task in the schedule")

    def fiterAndMerge(seq: Seq[ScheduledJob]) = mergeScheduledJobs(seq.filter(_.isOfTask(task)))

    jobsStream.map(fiterAndMerge).flatten
  }

  def printSchedule(): Unit = {
    jobsStream map mergeScheduledJobs foreach println
  }

  def jobStream(idx: Int): Seq[ScheduledJob] = {
    require(idx < jobsStream.size)
    mergeScheduledJobs(jobsStream(idx))
  }

  def findUncompletedJobs(): Seq[Seq[Job]] = jobsStream.map(findUncompletedJobs)

  def findUncompletedJobs(idx: Int): Seq[Job] = {
    require(idx < jobsStream.size)
    findUncompletedJobs(jobsStream(idx))
  }

  def findOverdueJobs(): Seq[Seq[ScheduledJob]] = jobsStream.map(findOverdueJobs)

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

  /**
   * saves output of the schedule
   */
  private def toJobSequences(schedule: Schedule, limit: Int): Seq[Seq[ScheduledJob]] = {
    def takeWhileBusy(schedule: Schedule): List[Seq[ScheduledJob]] =
      if (schedule.isBusy)
        schedule.next() :: takeWhileBusy(schedule)
      else
        Nil

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
}
