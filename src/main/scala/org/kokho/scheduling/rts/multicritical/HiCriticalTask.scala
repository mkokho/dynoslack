package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

trait HiCriticalTask extends MulticriticalTask with SynchronousTask {

  override type JobType = HiCriticalJob

  def hiExecution: Int

  def loExecution: Int

  def lowJobs: Int => Boolean

  override def execution: Int = hiExecution

  override def toString: String = s"Hi($period, $loExecution, $hiExecution)"
}


object HiCriticalTask {
  def apply(name: String, period: Int, loExecution: Int, hiExecution: Int): HiCriticalTask =
    apply(name, period, loExecution, hiExecution, _ => false)


  def apply(name_ : String, period: Int, loExecution: Int, hiExecution: Int, lowJobs: Int => Boolean): HiCriticalTask =
    new HiCriticalTaskDefault(period, loExecution, hiExecution, lowJobs)
      with NamedTask {
      override def name: String = name_
    }

  def apply(period: Int, loExecution: Int, hiExecution: Int): HiCriticalTask =
    apply(period, loExecution, hiExecution, _ => false)

  def apply(period: Int, loExecution: Int, hiExecution: Int, lowJobs: Int => Boolean): HiCriticalTask =
    new HiCriticalTaskDefault(period, loExecution, hiExecution, lowJobs)

}
