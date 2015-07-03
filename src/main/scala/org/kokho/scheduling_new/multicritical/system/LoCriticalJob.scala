package org.kokho.scheduling_new.multicritical.system

import org.kokho.scheduling_new.{PeriodicJob, Task}


/**
 * Represents a job of a LowCriticalTask
 *
 * @author: Mikhail Kokho
 * @date: 6/4/15
 */
case class LoCriticalJob(release: Int, task: LoCriticalTask) extends PeriodicJob {

  override def isOfTask(thatTask: Task): Boolean = this.task match {
    case t: LoCriticalTaskDefault => thatTask == t || t.relatedTo.getOrElse(t) == thatTask
    case _ => this.task == thatTask
  }

}
