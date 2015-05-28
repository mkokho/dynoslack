package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait PeriodicTaskBehavior { this: FlatSpec =>

  def aPeriodicTask(task: PeriodicTask): Unit = {

    it must "have the deadline equal to the period" in {
      assert(task.deadline == task.period)
    }

    it must "release next job immediately after the deadline of the last job" in {
      val jobPairs = task.jobs().take(100).sliding(2)
      for(job :: nextJob :: Nil <- jobPairs){
        assert(job.deadline == nextJob.release )
      }
    }

  }

}
