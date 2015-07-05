package org.kokho.scheduling

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by Mikhail Kokho on 7/3/2015.
 */
class JobTest extends FunSuite with Matchers{
  val job = Job(0, 2, 10)

  test("Job() returns what expected") {
    job.release shouldEqual 0
    job.length shouldEqual 2
    job.deadline shouldEqual 10
    job.releasedBy.isEmpty shouldBe true
  }

  test("JobProxy is correct") {
    val jobProxy = Job(job)
    jobProxy.length shouldEqual job.length
    jobProxy.deadline shouldEqual job.deadline
    jobProxy.release shouldEqual job.release
    jobProxy.releasedBy shouldEqual job.releasedBy
  }

  test("IdleJob test") {
    assert(IdleJob.toString.nonEmpty)
  }

}
