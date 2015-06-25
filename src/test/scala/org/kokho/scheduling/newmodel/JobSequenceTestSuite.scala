package org.kokho.scheduling.newmodel

import org.kokho.scheduling.rts.multicritical.HiCriticalTask
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitSuite

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
class JobSequenceTestSuite extends FlatSpec with JobSequenceBehavior{

  val taskA = HiCriticalTask(9, 5, 5)
  val taskB = HiCriticalTask(7, 5, 5)
  val taskC = HiCriticalTask(3, 1, 1)

  "A Job Sequence of one task" must behave like aJobSequence(JobSequence(taskA))

  "A merged job sequence" must behave like aJobSequence(JobSequence(taskA).merge(JobSequence(taskB)))

  "A merged job sequence of three tasks" must behave like aJobSequence(JobSequence(taskA).merge(JobSequence(taskB)).merge(JobSequence(taskC)))


}
