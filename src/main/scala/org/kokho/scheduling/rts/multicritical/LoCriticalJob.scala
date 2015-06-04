package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{JobProxy, Job, PeriodicJob}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */
case class LoCriticalJob(private val task: LoCriticalTask, job: PeriodicJob) extends JobProxy {

//  def this(task: LoCriticalTask, job: PeriodicJob) = this(task, job.release, job.length, job.deadline)

}
