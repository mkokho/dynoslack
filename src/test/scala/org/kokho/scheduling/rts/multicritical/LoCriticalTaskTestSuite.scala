package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicTaskBehavior, TaskBehavior}
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTaskTestSuite extends FlatSpec with TaskBehavior with PeriodicTaskBehavior{

  def loCriticalTask = new LoCriticalTask(10, 4, List(6,8))

  "A low critical task" should behave like aTask(loCriticalTask)

  it should behave like aPeriodicTask(loCriticalTask)

}
