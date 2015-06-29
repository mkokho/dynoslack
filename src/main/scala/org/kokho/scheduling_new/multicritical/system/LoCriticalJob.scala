package org.kokho.scheduling_new.multicritical.system

import org.kokho.scheduling_new.PeriodicJob


/**
 * Represents a job of a LowCriticalTask
 *
 * @author: Mikhail Kokho
 * @date: 6/4/15
 */
case class LoCriticalJob(release: Int, task: LoCriticalTask) extends PeriodicJob {


}
