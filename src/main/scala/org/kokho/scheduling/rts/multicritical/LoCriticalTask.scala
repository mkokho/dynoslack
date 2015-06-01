package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.PeriodicTask.PeriodicJob
import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTask (val period:Int, val loExecution:Int, val earlyReleases: List[Int]) extends MulticriticalTask{

  override type JobType = LoCriticalJob

  def this(period: Int, loExecution: Int) = this(period, loExecution, loExecution.until(period).toList)

  override def execution: Int = loExecution

  override def convertJob(job: PeriodicJob): JobType = LoCriticalJob(this, job)
}


case class LoCriticalJob(private val task: LoCriticalTask, job: PeriodicJob) extends ForwardingJob{
}
