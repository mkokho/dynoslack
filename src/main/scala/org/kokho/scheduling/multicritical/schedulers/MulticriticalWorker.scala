package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.JobStream._
import org.kokho.scheduling.multicritical.system.{LoCriticalTask, MulticriticalTask}
import org.kokho.scheduling.{Job, ScheduledJob, Task}

/**
 * Mutable class that handles management of early releases
 *
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
class MulticriticalWorker(private var tasks: Seq[MulticriticalTask]) extends EdfHiFirstOrdering{

  private var schedule: FunSchedule = FunSchedule(tasks)

  def releaseLocally() = tasksForEarlyRelease map { loTask =>
    val demand = loTask.demand(currentTime)
    val deadline = currentTime + loTask.period

    if (slackUntil(deadline) >= demand) {
      val earlyJob = extractEarlyJob(loTask)
      insertEarlyJob(earlyJob)
    }
  }

  def tasksForEarlyRelease =
    for {
      loTask: LoCriticalTask <- tasks collect { case t: LoCriticalTask => t}
      if loTask.canReleaseEarlyJob(currentTime)
      if notActive(loTask)
    } yield loTask

  def notActive(task: Task) = !isActive(task)

  def isActive(task: Task) = schedule.isActive(task)

  def slackUntil(to: Int) = slackBetween(currentTime, to)

  def slackBetween(from: Int, to: Int) =
    schedule
      .slackStream(to)
      .dropWhile(_.start < from)
      .length

  def extractEarlyJob(loTask: LoCriticalTask): Job = {
    //shift the task and release an early job
    val nextLoTask = loTask.shift(currentTime)
    val earlyJob = nextLoTask.jobs(currentTime).next()

    //update the task in the mutable variable
    val idx = tasks.indexOf(loTask)
    tasks = tasks.updated(idx, nextLoTask)

    //update jobs streams in our FunSchedule
    //note that we exclude the released job from this schedule
    schedule = schedule.update(toJobStream(tasks).remove(earlyJob))

    //return the released job
    earlyJob
  }

  def insertEarlyJob(earlyJob: Job) = {
    assert(earlyJob.release >= currentTime)
    val newJobStream = schedule.jobStream.insert(earlyJob)
    schedule = schedule.update(newJobStream)
  }

  def currentTime = schedule.time

  def next(): ScheduledJob = {
    val job = schedule.scheduledJob
    schedule = schedule.nextState()
    job
  }


}
