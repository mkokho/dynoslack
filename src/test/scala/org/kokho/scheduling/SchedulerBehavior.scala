package org.kokho.scheduling

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait SchedulerBehavior extends Matchers{
  this: FlatSpec =>

  def aNonEmptyScheduler(scheduler: Scheduler) = {

    it must "have a non empty set of tasks" in {
      scheduler.tasks.nonEmpty shouldBe true
    }

    it must "generate non-empty schedule" in {
        scheduler.iterate().nonEmpty shouldBe true
    }

    it must "schedule jobs of the tasks" in {
      val schJob = scheduler.iterate().dropWhile(_.forall(_.isIdle)).next().filter(!_.isIdle)(0)
      val job = schJob.scheduledJob

      schJob.isIdle shouldBe false
      schJob.toString.isEmpty shouldBe false

      scheduler.tasks.exists(t => job.isOfTask(t)) shouldBe true
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
