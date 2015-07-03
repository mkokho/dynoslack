package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.ScheduledJob
import org.kokho.scheduling_new.multicritical.system.MulticriticalTask

/**
 * @author: Mikhail Kokho
 * @date 7/3/15.
 */
class SwapScheduler (override val partition: Seq[Seq[MulticriticalTask]]) extends MulticriticalScheduler(partition){
  self =>
  /**
   * Infinite iterator over a sequence of scheduled jobs
   */
  override def iterate(): Iterator[Seq[ScheduledJob]] = ???
}
