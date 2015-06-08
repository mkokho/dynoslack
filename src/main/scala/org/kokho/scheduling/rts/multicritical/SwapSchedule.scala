package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduledJob

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */

final class SwapSchedule(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition) {

  override def next(): Seq[ScheduledJob] = {
    localSchedules.map(itr => itr.next())
  }

}
