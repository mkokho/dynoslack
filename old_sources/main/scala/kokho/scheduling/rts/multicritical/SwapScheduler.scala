package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapScheduler() extends Scheduler {

  override type AllowedTasks = MulticriticalTask

  override def schedule(partition: Seq[Set[MulticriticalTask]]): Schedule = {
    val overutilizedSets = partition.filter(_.map(_.utilization).sum > 1)
    if (overutilizedSets.size > 0) {
      throw new UnschedulableSetException(
        s"Cannot schedule sets of tasks with total utilization > 1: $overutilizedSets"
      )
    }

    new SwapSchedule(partition.map(_.toSeq))
  }
}



