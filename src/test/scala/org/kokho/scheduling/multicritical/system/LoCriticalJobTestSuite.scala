package org.kokho.scheduling.multicritical.system

import org.kokho.scheduling.JobBehavior
import org.scalatest.FlatSpec

/**
 * @author: Mikhail Kokho
 * @date: 7/3/2015
 */
class LoCriticalJobTestSuite extends FlatSpec with JobBehavior {

  val taskA = HiCriticalTask(10, 4, 4)
  val taskL = LoCriticalTask(10, 2)
  val shiftedTaskCorrect = taskL.shift(5)
  val shiftedTaskIncorrect = taskL.shift(6)

  val job = taskL.shift(5).job(0)

  "A LoCriticalJob" must behave like aJob(job)

  it should "have correct implementation of isOfTask method" in {
    job.isOfTask(taskA) shouldBe false
    job.isOfTask(taskL) shouldBe true
    job.isOfTask(shiftedTaskCorrect) shouldBe true
    job.isOfTask(shiftedTaskIncorrect) shouldBe false
  }

}
