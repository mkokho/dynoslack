package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait SchedulerBehavior {
  this: FlatSpec =>

  def aScheduler(scheduler: Scheduler) = {

    it must "generate an empty schedule when a set of tasks is empty" in {
      if (scheduler.tasks.isEmpty)
        assert(scheduler.iterate().isEmpty)
    }

    it must "generate non-empty schedule when a set of tasks is not empty" in {
      if (scheduler.tasks.nonEmpty)
        assert(scheduler.iterate().nonEmpty)
    }

  }

  def aSchedulerWithOneTaskOnOneCore[T <: Task](schedule: Schedule, partition: Seq[Set[T]]): Unit = {
    it should "contain one task" in {
      assert(partition.flatten.size == 1)
    }
    
    it should "have one core" in {
      assert(partition.size == 1)
    }

    it must "periodically execute the task" in {
      val task = partition.flatten.head
      //get schedule on the core
      0.to(1) foreach { _ =>
        val flatSchedule = schedule.flatten.take(task.period).toList.map(_.scheduledJob)
        val execution = flatSchedule.takeWhile(_ != IdleJob).size
        assert(execution == task.execution)

        val idleTime = flatSchedule.drop(execution).takeWhile(_ == IdleJob).size
        assert(idleTime == task.period - task.execution)
      }
    }
  }


}
