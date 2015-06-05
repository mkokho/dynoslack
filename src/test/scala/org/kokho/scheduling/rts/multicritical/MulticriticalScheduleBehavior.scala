package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{ScheduleBehavior, Schedule}
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/5/15.
 * @author: Mikhail Kokho
 */
trait MulticriticalScheduleBehavior extends ScheduleBehavior{
  this: FlatSpec =>

  implicit def partitionToSchedule(partition: Seq[Seq[MulticriticalTask]]): Schedule
  
  def aMulticriticalSchedule(sch: MulticriticalSchedule): Unit ={
    
    it should behave like aSchedule(sch)
    
  }

}
