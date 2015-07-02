package org.kokho.scheduling_new

import org.kokho.scheduling_new.multicritical.system.HiCriticalTask
import org.scalatest.FlatSpec
import JobStream._

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
class JobStreamTestSuite extends FlatSpec with JobStreamBehavior{

  val taskA = HiCriticalTask(9, 5, 5)
  val taskB = HiCriticalTask(7, 5, 5)
  val taskC = HiCriticalTask(3, 1, 1)

  "A Job Sequence of one task" must behave like aJobStream(taskA)

  "A merged job sequence" must behave like aJobStream(taskA.merge(taskB))

  "A merged job sequence of three tasks" must behave like aJobStream(Seq(taskA, taskB, taskC))


}
