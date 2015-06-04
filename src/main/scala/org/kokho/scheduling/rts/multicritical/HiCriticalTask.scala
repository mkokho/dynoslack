package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

trait HiCriticalTask extends MulticriticalTask with SynchronousTask{

  override type JobType = HiCriticalJob

  def hiExecution: Int

  def loExecution: Int

  def lowJobs: Int => Boolean

  override def execution: Int = hiExecution
}


object HiCriticalTask {

  def apply(period:Int, loExecution:Int, hiExecution: Int, lowJobs: Int => Boolean) =
    new HiCriticalTaskDefault(period, loExecution, hiExecution, lowJobs)

  def apply(period:Int, loExecution:Int, hiExecution: Int) =
    new HiCriticalTaskDefault(period, loExecution, hiExecution, _ => false)

}
