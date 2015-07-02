package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.ScheduledJob
import org.kokho.scheduling_new.multicritical.system.{LoCriticalTask, MulticriticalTask}

/**
 * Mutable class that handles management of early releases
 *
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
class MulticriticalWorker(private var tasks: Seq[MulticriticalTask]) extends EdfHiFirstOrdering {

  private var schedule: FunSchedule = FunSchedule(tasks)

  def tasksForEarlyRelease =
    for {
      loTask:LoCriticalTask <- tasks collect { case t: LoCriticalTask => t}
      if loTask.canReleaseEarlyJob(currentTime)
      if !schedule.isActive(loTask)
    } yield loTask

  def isLocalPossible(loTask: LoCriticalTask) = tasks.contains(loTask) && {
    val deadline = currentTime + loTask.period
    val demand = loTask.demand(currentTime)
    schedule.availableSlack(deadline) >= demand
  }

  def currentTime = schedule.time

  def releaseEarlyJob(loTask: LoCriticalTask) = {
    val idx = tasks.indexOf(loTask)
    tasks = tasks.updated(idx, loTask.shift(currentTime))
    schedule = schedule.update(tasks)
  }

  def next(): ScheduledJob = {
    val job = schedule.scheduledJob
    schedule = schedule.nextState()
    job
  }


}
