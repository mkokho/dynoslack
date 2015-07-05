package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{JobProxy, PeriodicJob, Task}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */
case class LoCriticalJob(private val task: LoCriticalTask, job: PeriodicJob) extends JobProxy {
  override def isOfTask(thatTask: Task): Boolean = thatTask match {
    case that: LoCriticalTask => this.task == thatTask || this.task.isChildOf(that)
    case _ => false
  }
}
