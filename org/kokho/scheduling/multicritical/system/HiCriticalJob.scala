package org.kokho.scheduling.multicritical.system

import org.kokho.scheduling.PeriodicJob

/**
 * Represents a job of a HiCriticalTask
 *
 * @author: Mikhail Kokho
 * @date 6/4/15.
 */
case class HiCriticalJob(release: Int, task: HiCriticalTask) extends PeriodicJob{

  val hiWcet = task.hiExecution

  val loWcet = task.loExecution

  override def length = if (takeLowWcet) loWcet else hiWcet

  def takeLowWcet: Boolean = task.lowJobs(this.index)
}

