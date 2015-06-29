package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling.rts.multicritical.HiCriticalTask
import org.kokho.scheduling_new.{Job, JobStream, Task, Scheduler}
import org.kokho.scheduling_new.multicritical.system.{LoCriticalTask, MulticriticalTask}

/**
 * @author: Mikhail Kokho
 * @date 6/29/15.
 */
abstract class MulticriticalScheduler(val partition: Seq[Seq[MulticriticalTask]]) extends Scheduler{


  override type AllowedTasks = MulticriticalTask

  /**
   * The number of processors used by the scheduler
   */
  override def arity: Int = partition.size

  /**
   * Low-criticality tasks that are being scheduled by this scheduler
   */
  def loTasks: Seq[LoCriticalTask] = tasks collect { case task: LoCriticalTask => task}

  /**
   * Tasks that are being scheduled
   */
  override def tasks: Seq[MulticriticalTask] = partition.flatten

  /**
   * High-criticality tasks that are being scheduled by this scheduler
   */
  def hiTasks: Seq[HiCriticalTask] = tasks collect { case task: HiCriticalTask => task}


  def toJobStream(task: Task) = new JobStream {
    override def produce(): Iterator[Job] = task.jobs()

    override def produce(from: Int): Iterator[Job] = task.jobs(from)
  }
}
