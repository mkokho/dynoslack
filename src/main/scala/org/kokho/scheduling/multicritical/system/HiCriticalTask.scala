package org.kokho.scheduling.multicritical.system

import org.kokho.scheduling._

/**
 * Represent a high-criticality tasks in the Elastic Mixed-Criticality model of real-time system
 *
 * @author: Mikhail Kokho
 * @date: 5/28/15
 */
trait HiCriticalTask extends MulticriticalTask{

  override type JobType = HiCriticalJob

  def hiExecution: Int

  def loExecution: Int

  /**
   * A set of jobs that take less time at runtime
   */
  def lowJobs: Int => Boolean

  override def execution: Int = hiExecution

  override def offset: Int = 0

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
