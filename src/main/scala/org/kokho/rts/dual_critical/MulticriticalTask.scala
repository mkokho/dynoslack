package main.scala.org.kokho.rts.dual_critical

import main.scala.org.kokho.rts.dual_critical.Criticality.Criticality

/**
 * Created by Misha on 2/15/2015.
 */


object Criticality extends Enumeration{
  type Criticality = Value

  val LO, HI = Value
}

trait MulticriticalTask {

  def execution: Criticality => Int

}

