package org.kokho.scheduling

/**
 * Created by Mikhail Kokho on 6/7/2015.
 */
class ScheduleAnalyzer(val schedule: Schedule, val memory: Int = 30) {
  private val jobsStream = toJobSequences(schedule, memory)

  def findJobs(task: Task):Seq[ScheduledJob] = {
    require(schedule.tasks.contains(task), "There is no such task in the schedule")

    def fiterAndMerge(seq: Seq[ScheduledJob]) = mergeScheduledJobs(seq.filter(_.isOfTask(task)))

    jobsStream.map(fiterAndMerge).flatten
  }

  def printSchedule(): Unit = {
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
