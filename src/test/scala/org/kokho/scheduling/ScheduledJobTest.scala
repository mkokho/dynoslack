package org.kokho.scheduling

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by Mikhail Kokho on 7/3/2015.
 */
class ScheduledJobTest extends FunSuite with Matchers{

  val job = Job(0,3,6)
  val leftJ = new ScheduledJob(0,1,job)
  val middleJ = new ScheduledJob(1,3,job)
  val rightJ = new ScheduledJob(3,4,job)
  val other = new ScheduledJob(4, 5, Job(3,3,6))

  test("consecutive method"){
    leftJ.isConsecutive(middleJ) shouldBe true
    rightJ.isConsecutive(middleJ) shouldBe true
    rightJ.isConsecutive(other) shouldBe false
  }

  test("merge method") {
    intercept[IllegalArgumentException] {
      leftJ.merge(rightJ)
    }

    intercept[IllegalArgumentException] {
      rightJ.merge(other)
    }

    leftJ.merge(middleJ).length shouldBe leftJ.length + middleJ.length
    rightJ.merge(middleJ).length shouldBe rightJ.length + middleJ.length
  }

  test("overdue method") {
    val deadline = job.deadline
    new ScheduledJob(deadline, deadline + 1, job).isOverdue shouldBe true
  }

}
