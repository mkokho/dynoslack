package org.kokho.scheduling.newmodel

import org.kokho.scheduling.Job
import org.kokho.scheduling.rts.multicritical.LoCriticalTask

/**
 * Created with IntelliJ IDEA on 6/26/15.
 * @author: Mikhail Kokho
 */
class JobSequenceWithER(val loTask: LoCriticalTask) extends JobSequence {

  private var curTask = loTask

  def currentTask = curTask

  /**
   * Returns jobs in order of their release times
   */
  override def produce(): Iterator[Job] = curTask.jobs()

  def releaseEarly(time: Int) = {
    require(curTask.canReleaseEarlyJob(time), s"Task $curTask cannot release an early job at time $time")
    curTask = curTask.shiftedTasks(time)
  }
}
