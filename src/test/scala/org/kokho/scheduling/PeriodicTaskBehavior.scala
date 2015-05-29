package org.kokho.scheduling

import org.scalatest.FlatSpec

import scala.util.Random

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait PeriodicTaskBehavior {
  this: FlatSpec =>

  def aPeriodicTask(task: PeriodicTask): Unit = {

    it must "have the deadline equal to the period" in {
      assert(task.deadline == task.period)
    }

    it must "release jobs with length and deadline equal to the task's parameters" in {
      val job = task.jobs().next()
      assert(job.deadline == task.offset + task.period)
      assert(job.length == task.execution)
    }

    it must "release next job immediately after the deadline of the last job" in {
      val jobPairs = task.jobs().take(100).sliding(2)
      for (job :: nextJob :: Nil <- jobPairs) {
        assert(job.deadline == nextJob.release)
      }
    }

    it must "produce equal jobs no matter where it starts" in {
      def isEqual[T](a: Iterator[T], b: Iterator[T], limit: Int = 100): Boolean =
        if (limit == 0) true
        else {
          val jobA = a.next()
          val jobB = b.next()
          jobA == jobB && isEqual(a, b, limit - 1)
        }

      // random start
      val r = Random.nextInt(1 << 10)
      assert(isEqual(task.jobs(r), task.jobs(r)))

      var itr = task.jobs()
      var itrAhead = task.jobs(1)

      //iterator that is 1 second behind must syncronize with the iterator that is ahead after one iteration
      itr.next()
      assert(isEqual(itr, itrAhead))

      //one iterator is exactly behind for one period
      itr = task.jobs()
      itr.next()
      itrAhead = task.jobs(task.period)
      assert(isEqual(itr, itrAhead))

    }

  }

}
