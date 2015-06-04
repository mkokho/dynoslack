package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicJob, JobProxy}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 *
 * @param task
 * @param job
 */
case class HiCriticalJob(private val task: HiCriticalTask, job: PeriodicJob) extends JobProxy{

  val hiWcet = task.hiExecution

  val loWcet = task.loExecution

  def takeLowWcet: Boolean = task.lowJobs(this.job.idx)

  override def length = if (takeLowWcet) loWcet else hiWcet
}
