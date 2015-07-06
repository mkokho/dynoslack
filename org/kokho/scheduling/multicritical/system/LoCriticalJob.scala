package org.kokho.scheduling.multicritical.system

import org.kokho.scheduling.{PeriodicJob, Task}


/**
 * Represents a job of a LowCriticalTask
 *
 * @author: Mikhail Kokho
 * @date: 6/4/15
 */
case class LoCriticalJob(release: Int, task: LoCriticalTaskDefault) extends PeriodicJob {

  override def isOfTask(thatTask: Task): Boolean =
    thatTask == this.task || this.task.relatedTo.getOrElse(this.task) == thatTask

}
