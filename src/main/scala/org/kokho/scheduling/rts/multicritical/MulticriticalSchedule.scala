package org.kokho.scheduling.rts.multicritical

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

  require(partition.count(_.map(_.utilization).sum > 1) == 0, "Cannot schedule a set of tasks with utilization > 1")

  protected val loTasks: Seq[LoCriticalTask] = tasks collect { case task: LoCriticalTask => task}
  protected val hiTasks: Seq[HiCriticalTask] = tasks collect { case task: HiCriticalTask => task}
  protected val localSchedules: Seq[LocalSchedule] = partition.map(new LocalSchedule(_)).toIndexedSeq


  if (tasks.size != loTasks.size + hiTasks.size) {
    val unsupportedClasses = tasks.diff(loTasks).diff(hiTasks).map(_.getClass.getCanonicalName)
    throw new IllegalArgumentException(
      "Only tasks of LoCriticalTask or HiCriticalTasks classes are supported" +
        s"Given classes: $unsupportedClasses"
    )
  }

  def taskToLocalSchedule(task: Task): Option[LocalSchedule] =
    localSchedules collectFirst {case sch if sch.isHost(task) => sch}

  /**
   * True if there is an unfinished job on one of the processors
   */
  override def isBusy: Boolean = localSchedules.count(_.isBusy) > 0
}