package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class HiCriticalTask (val period:Int, val loExecution:Int, val hiExecution: Int) extends Task
with SynchronousTask
with ImplicitDeadlineTask
with PeriodicTask
{
  override type JobType = HiCriticalJob

  override def execution: Int = hiExecution

  override def convertJob(job: Job): JobType = HiCriticalJob(this, job)
}


case class HiCriticalJob(private val task: HiCriticalTask, job: Job) extends JobDecorator(job){

  val hiWcet = task.hiExecution

  val loWcet = task.loExecution

}
