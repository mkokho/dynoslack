package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.Job
import org.kokho.scheduling_new.multicritical.system.{HiCriticalJob, LoCriticalJob}

/**
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
trait EdfHiFirstOrdering {

  implicit def edfOrdering: Ordering[Job] = new Ordering[Job] {
    override def compare(x: Job, y: Job): Int = {
      val res = x.deadline - y.deadline
      if (res == 0) {
        (x, y) match {
          case (_: LoCriticalJob, _: HiCriticalJob) => 1
          case (_: HiCriticalJob, _: LoCriticalJob) => -1
          case _ => 0
        }
      } else
        res
    }
  }

}
