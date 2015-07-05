package org.kokho.scheduling

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Mikhail Kokho on 7/3/2015.
 */
trait JobBehavior extends Matchers{
  self: FlatSpec =>

  def aJob(job: Job): Unit = {

    it should "not throw exception when converted to string" in {
      try {
        job.toString
      } catch {
        case e: Exception => fail(e)
      }
    }

    it should "have correct relative deadline" in {
      job.relativeDeadline shouldEqual (job.deadline - job.release)
    }
  }
}