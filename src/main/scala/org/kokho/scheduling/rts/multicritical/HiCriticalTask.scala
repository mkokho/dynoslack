package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.PeriodicTask.PeriodicJob
import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 *
 * @param period - period of execution
 * @param loExecution - execution time of a job in low-critical mode
 * @param hiExecution - execution time of a job in high-critical mode
 * @param lowJobs - set of indexes of jobs which are executed in low-critical mode.
 *                The jobs are indexed from 0.
 */
class HiCriticalTask (val period:Int, val loExecution:Int, val hiExecution: Int,
                       val lowJobs: Int => Boolean = {_ => false})
  extends MulticriticalTask{

  override type JobType = HiCriticalJob

  override def execution: Int = hiExecution

  override def convertJob(job: PeriodicJob): JobType = HiCriticalJob(this, job)
}


case class HiCriticalJob(private val task: HiCriticalTask, job: PeriodicJob) extends ForwardingJob{

  val hiWcet = task.hiExecution

  val loWcet = task.loExecution

  def takeLowWcet: Boolean = task.lowJobs(this.job.idx)

  override def length = if (takeLowWcet) loWcet else hiWcet
}
