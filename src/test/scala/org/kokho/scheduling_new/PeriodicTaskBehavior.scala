package org.kokho.scheduling_new

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait PeriodicTaskBehavior extends Matchers{
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
      // random start
      val r = Random.nextInt(1 << 10)
      // size to compare
      val size = 100

      task.jobs(r).take(size).toList shouldEqual task.jobs(r).take(size).toList

    }

    it must "produce two job iterators, one being ahead, that produce equal jobs from some moment of time" in {
      val size = 1
      var itr = task.jobs().drop(1).take(size).toList
      var itrAhead = task.jobs(task.offset + 1).take(size).toList

      //iterator that is 1 second behind must syncronize with the iterator that is ahead after one iteration
      itr shouldEqual itrAhead

      //one iterator is exactly behind for one period
      itr = task.jobs().drop(1).take(size).toList
      itrAhead = task.jobs(task.offset + task.period).take(size).toList

      itr shouldEqual itrAhead
    }

  }

}
