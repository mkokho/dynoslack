package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.multicritical.system.{LoCriticalTask, MulticriticalTask}
import org.kokho.scheduling_new.{ScheduledJob, Job, JobStream, Task}

/**
 * Mutable class that handles management of early releases
 *
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
class MulticriticalSchedule(private var tasks: Seq[MulticriticalTask]) extends EdfOrdering{

  private var schedule: SchedulerHelper = SchedulerHelper(tasks)

  def tasksForEarlyRelease = tasks.collect {case loTask: LoCriticalTask => loTask}.filter(_.canReleaseEarlyJob(currentTime))

  def isLocalPossible(loTask: LoCriticalTask) = tasks.contains(loTask) && {
    val deadline = currentTime + loTask.period
    val demand = loTask.demand(currentTime)
    schedule.availableSlack(deadline) >= demand
  }

  def currentTime = schedule.time

  def releaseEarlyJob(loTask: LoCriticalTask) = {
    val idx  = tasks.indexOf(loTask)
    tasks = tasks.updated(idx, loTask.shift(currentTime))
    schedule = SchedulerHelper(tasks)
  }

  def next():ScheduledJob = {
    val job = schedule.scheduledJob
    schedule = schedule.nextState()
    job
  }


  implicit def toJobStream(ts: Seq[Task]): JobStream = ts.map(toJobStream).reduce(_ merge _)

  def toJobStream(task: Task): JobStream = new JobStream {
    override def produce(): Iterator[Job] = task.jobs()

    override def produce(from: Int): Iterator[Job] = task.jobs(from)
  }

}
