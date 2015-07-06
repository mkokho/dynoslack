package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.Scheduler
import org.kokho.scheduling.multicritical.system.MulticriticalTask

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
   * Tasks that are being scheduled
   */
  override def tasks: Seq[MulticriticalTask] = partition.flatten

}
