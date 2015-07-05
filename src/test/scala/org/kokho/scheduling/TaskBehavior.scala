package org.kokho.scheduling

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait TaskBehavior extends Matchers{
  this: FlatSpec =>

  def aTask(task: Task) {
    it must "have positive execution, deadline and period" in {
      assert(task.execution > 0)
      assert(task.deadline > 0)
      assert(task.period > 0)
    }

    it must "have non-negative offset" in {
      assert(task.offset >= 0)
    }

    it must "have utilization equal to execution/period" in {
      assert(task.utilization == (task.execution.toDouble / task.deadline))
    }

    it should "not fall when toString method is called" in {
      try{
        task.toString
      }catch {
        case e:Exception => fail("Method toString failed")
      }
    }

    it must "produce non empty sequence of jobs" in {
      assert(task.jobs().nonEmpty)
    }

    it must "produce equal first jobs" in {
      assert(task.jobs().next() == task.jobs().next())
    }

    it must "release jobs no earlier than the period" in {
      val jobs = task.jobs()
      val j1 = jobs.next()
      val j2 = jobs.next()

      assert(j2.release >= j1.release + task.period, "Second Job must be released no earlier than T.p units of time")
    }

    it must "release the first job at the moment offset" in {
      assert(task.jobs().next().release == task.offset,
        "The release time of the first job does not equal offset of the task")
    }

    it must "have first job in its iterator equal to the job with index 0" in {
      task.jobs().next() shouldEqual task.job(0)
    }

    it must "produce jobs in order of their release times" in {
      val jobs = task.jobs().buffered
      var jobPrev = jobs.next()
      for (i <- Range(0, 1000)) {
        assertResult(jobPrev.deadline) {
          jobs.head.release
        }
        jobPrev = jobs.next()
      }
    }

  }

}
