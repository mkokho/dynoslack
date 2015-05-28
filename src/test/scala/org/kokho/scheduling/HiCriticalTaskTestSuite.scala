package org.kokho.scheduling

import org.kokho.scheduling.rts.multicritical.HiCriticalTask
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class HiCriticalTaskTestSuite extends FlatSpec with TaskBehavior with PeriodicTaskBehavior{

  def hiCriticalTask = new HiCriticalTask(10, 4, 6)

  "A high critical task" should behave like aTask(hiCriticalTask)

  it should behave like aPeriodicTask(hiCriticalTask)

}
