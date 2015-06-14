package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.MulticriticalTask

/**
 * Created by Mikhail Kokho on 6/14/2015.
 */
class FixedTaskGenerator(val taskset: Seq[MulticriticalTask]) extends TaskGenerator{

  private var tasksetItr = taskset.iterator

  override def generateMulticriticalTask(): MulticriticalTask = {
    if (tasksetItr.isEmpty) tasksetItr = taskset.iterator
    tasksetItr.next()
  }
}
