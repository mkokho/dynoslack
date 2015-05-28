package org.kokho.scheduling

import org.kokho.scheduling.exceptions.UnschedulableSetException
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait SchedulerBehavior {
  this: FlatSpec =>

  def aScheduler(scheduler: Scheduler) = {
    it must "generate an empty schedule when a set of tasks is empty" in {
      if (scheduler.tasks.isEmpty)
        assert(scheduler.schedule().isEmpty)
    }

    it must "generate non-empty schedule when a set of tasks is not empty" in {
      if (scheduler.tasks.nonEmpty)
        assert(scheduler.schedule().nonEmpty)
    }

    it must "produce UnschedulableSetException when utilization of tasks exceeds the amount of cores" in {
      val totalUtilization = scheduler.tasks.map(_.utilization).sum
      if (totalUtilization > scheduler.cores.size) {
        intercept[UnschedulableSetException] {
          scheduler.schedule()
        }
      }
    }

  }
}
