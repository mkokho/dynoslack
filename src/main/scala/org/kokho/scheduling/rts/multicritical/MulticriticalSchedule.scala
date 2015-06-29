package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.newmodel.{JobSequence, JobSequenceWithER}
import org.kokho.scheduling.{Schedule, Task}

/**
 * Created with IntelliJ IDEA on 6/5/15.
 * @author: Mikhail Kokho
 */

/**
 * A base class for any schedule of multicritical tasks
 *
 * @param partition partition of tasks we want to schedule
 */
abstract class MulticriticalSchedule(val partition: Seq[Seq[MulticriticalTask]]) extends Schedule{

  require(partition.nonEmpty, "Partition must not be empty")
  
  require(partition.count(_.map(_.utilization).sum > 1) == 0, "Cannot schedule a set of tasks with utilization > 1")

/*
  require(tasks.size == loTasks.size + hiTasks.size, {
    val unsupportedClasses = tasks.diff(loTasks).diff(hiTasks).map(_.getClass.getCanonicalName)
      "Only tasks of LoCriticalTask or HiCriticalTasks classes are supported" +
        s"Given classes: $unsupportedClasses"
  })


  protected val jobStreams = tasks.map {
    case t: LoCriticalTask => (t, new JobSequenceWithER(t))
    case t: HiCriticalTask => (t, JobSequence(t))
  }.toMap*/

  protected val localSchedules: Seq[LocalSchedule] = partition.map(new LocalSchedule(_)).toIndexedSeq


  def hiTasks: Seq[HiCriticalTask] = tasks collect { case task: HiCriticalTask => task}

//  def tasksForEarlyRelease = loTasks.filter(canReleaseEarlyJob)

  def loTasks: Seq[LoCriticalTask] = tasks collect { case task: LoCriticalTask => task}

/*  def canReleaseEarlyJob(loTask: LoCriticalTask): Boolean = jobStreams.get(loTask) match {
    case Some(js: JobSequenceWithER) => !isActive(loTask) && js.currentTask.canReleaseEarlyJob(scheduleTime)
    case _ => false
  }*/

//  def scheduleTime: Int = schedules(0).time

//  def isActive(task: Task) = schedules.exists(_.isActive(task))

//  def schedules: Seq[ScheduleFun] = {
//    val mergedJobSequence = partition.map(_.map(jobStreams).reduce(_ merge _))
//    mergedJobSequence.map(new ScheduleFun(_))
//  }

  def taskToLocalSchedule(task: Task): Option[LocalSchedule] =
    localSchedules collectFirst {case sch if sch.isHost(task) => sch}

  /**
   * True if there is an unfinished job on one of the processors
   */
  override def isBusy: Boolean = localSchedules.count(_.isBusy) > 0
}
