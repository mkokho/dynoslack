package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class LowCriticalTask (val period:Int, val loExecution:Int, val earlyReleases: List[Int]) extends Task
with SynchronousTask
with ImplicitDeadlineTask
with PeriodicTask{
  override type JobType = LoCriticalJob

  override def execution: Int = loExecution

  override def convertJob(job: Job): JobType = LoCriticalJob(this, job)
}


case class LoCriticalJob(private val task: LowCriticalTask, job: Job) extends JobDecorator(job){
}
