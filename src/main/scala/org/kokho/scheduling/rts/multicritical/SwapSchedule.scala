package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{ScheduledJob, Schedule}

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */

final class SwapSchedule(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition) {

  val maxDeadline = if (loTasks.isEmpty) 0 else loTasks.map(_.deadline).max

  override def next(): Seq[ScheduledJob] = {
    localSchedules.map(itr => itr.next())
  }

}
