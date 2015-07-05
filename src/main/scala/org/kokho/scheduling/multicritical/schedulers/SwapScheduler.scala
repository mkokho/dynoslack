package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.ScheduledJob
import org.kokho.scheduling.multicritical.system.MulticriticalTask

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
