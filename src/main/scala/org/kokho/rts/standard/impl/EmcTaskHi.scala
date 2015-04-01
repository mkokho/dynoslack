package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model.{ImplicitDeadlineTask, Task, PeriodicTask, SynchronousTask}

/**
 * Created with IntelliJ IDEA on 4/1/15.
 * @author: Mikhail Kokho
 */
class EmcTaskHi private (override val name:String, val execution: Int, val period: Int, val earlyFinish:Int => Boolean) extends Task
with SynchronousTask with ImplicitDeadlineTask with PeriodicTask{

}

object EmcTaskHi {

  def apply(t:BasicTask, earlyFinish: Int => Boolean) = {
//    require(exec > 0, "Execution must be a positive number")
//    require(period > 0, "Period must be a positive number")
//    require(period >= exec, "Period must be greater or equal to execution")

    new EmcTaskHi(t.name, t.execution, t.period, earlyFinish)
  }

}
