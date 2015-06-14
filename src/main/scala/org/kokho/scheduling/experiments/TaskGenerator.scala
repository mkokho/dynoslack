package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.MulticriticalTask

/**
 * Created by Mikhail Kokho on 6/14/2015.
 */
trait TaskGenerator {
  def generateMulticriticalTask(): MulticriticalTask
}
