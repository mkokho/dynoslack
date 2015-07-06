package org.kokho.scheduling.multicritical.system

/**
 *
 * @param period - period of execution
 * @param loExecution - execution time of a job in low-critical mode
 * @param hiExecution - execution time of a job in high-critical mode
 * @param lowJobs - set of indexes of jobs which are executed in low-critical mode.
 *                The jobs are indexed from 0.
 *
 *
 * @author: Mikhail Kokho
 * @date: 6/4/15
 */

class HiCriticalTaskDefault (val period:Int,
                             val loExecution:Int,
                             val hiExecution: Int,
                             val lowJobs: Int => Boolean = {_ => false})
extends HiCriticalTask
{
  require(loExecution <= hiExecution, "Lo execution cannot be greater than hi execution")

  override def buildJob(release: Int): HiCriticalJob = HiCriticalJob(release, this)
}
