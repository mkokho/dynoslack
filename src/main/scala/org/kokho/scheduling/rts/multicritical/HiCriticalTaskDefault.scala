package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicTask, PeriodicJob}

/**
 * Created with IntelliJ IDEA on 6/4/15.
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

final class HiCriticalTaskDefault (val period:Int,
                             val loExecution:Int,
                             val hiExecution: Int,
                             val lowJobs: Int => Boolean = {_ => false})
extends HiCriticalTask
with PeriodicTask
{

  override def convertJob(job: PeriodicJob): JobType = HiCriticalJob(this, job)


}
