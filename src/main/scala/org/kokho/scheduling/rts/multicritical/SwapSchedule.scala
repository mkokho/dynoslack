package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{ScheduledJob, Schedule}

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */

final protected class SwapSchedule(val partition: Seq[Seq[MulticriticalTask]]) extends Schedule {
  require(partition.count(_.map(_.utilization).sum > 1) == 0, "Cannot schedule a set of tasks with utilization > 1")

  private val loTasks: Seq[LoCriticalTask] = tasks collect { case task: LoCriticalTask => task}
  private val hiTasks: Seq[HiCriticalTask] = tasks collect { case task: HiCriticalTask => task}
  private val localSchedules: Seq[LocalSchedule] = partition.map(new LocalSchedule(_))

  if (tasks.size != loTasks.size + hiTasks.size) {
    val unsupportedClasses = tasks.diff(loTasks).diff(hiTasks).map(_.getClass.getCanonicalName)
    throw new IllegalArgumentException(
      "Only tasks of LoCriticalTask or HiCriticalTasks classes are supported" +
        s"Given classes: $unsupportedClasses"
    )
  }

  val taskToLocalSchedule: Map[MulticriticalTask, LocalSchedule] = {
      for {
        idx <- 0.until(arity)
        task <- partition(idx)
      } yield task -> localSchedules(idx)
    }.toMap

  val maxDeadline = if (loTasks.isEmpty) 0 else loTasks.map(_.deadline).max

  private def releaseEarlyJobs(): Unit = {
    releaseEarlyJobsLocally()
  }

  private def releaseEarlyJobsGlobally() ={
    //tasks that can release an early jobs
    val tasksForER = localSchedules.map(_.tasksForEarlyRelease).flatten

    //we might want to order tasks
    //now we just use random fit
    for {
      task <- tasksForER
      scheduleWithSlack <- localSchedules if scheduleWithSlack.hasSlackForTask(task)
    } {
      val scheduleOfTask = taskToLocalSchedule.get(task).get
      val job = scheduleOfTask.releaseEarlyJob(task)
      scheduleWithSlack.insertJob(job)
    }

  }

  private def releaseEarlyJobsLocally() = {
    for {
      lSch <- localSchedules
      task <- lSch.tasksForEarlyRelease
      if lSch.hasSlackForTask(task)
    } {
      val job = lSch.releaseEarlyJob(task)
      lSch.insertJob(job)
    }
  }



  override def next(): Seq[ScheduledJob] = {
    releaseEarlyJobs()
    localSchedules.map(itr => itr.next())
  }

}
