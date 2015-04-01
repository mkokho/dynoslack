package main.scala.org.kokho.rts.standard.impl.tasks

import main.scala.org.kokho.rts.standard.model.{ImplicitDeadlineTask, PeriodicTask, SynchronousTask, Task}

/**
 * @author Mikhail Kokho
 */
class BasicTask private (override val name:String, val execution: Int, val period: Int)
  extends Task
  with SynchronousTask with ImplicitDeadlineTask with PeriodicTask{

}

object BasicTask {

  def apply(name:String, exec:Int, period: Int) = {
    require(exec > 0, "Execution must be a positive number")
    require(period > 0, "Period must be a positive number")
    require(period >= exec, "Period must be greater or equal to execution")

    new BasicTask(name, exec, period)
  }

}
