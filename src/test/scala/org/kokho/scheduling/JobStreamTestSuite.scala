package org.kokho.scheduling

import org.kokho.scheduling.multicritical.system.HiCriticalTask
import org.scalatest.FlatSpec
import JobStream._

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
class JobStreamTestSuite extends FlatSpec with JobStreamBehavior{

  "An Empty JobStream" must behave like anEmptyJobStream(JobStream.empty)

  val taskA = HiCriticalTask(9, 5, 5)
  val taskB = HiCriticalTask(7, 5, 5)
  val taskC = HiCriticalTask(3, 1, 1)

  "A Job Sequence of one task" must behave like aNonEmptyJobStream(taskA)

  "A merged job sequence" must behave like aNonEmptyJobStream(taskA.merge(taskB))

  "A merged job sequence of three tasks" must behave like aNonEmptyJobStream(Seq(taskA, taskB, taskC))


}
