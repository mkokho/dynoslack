package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait SchedulerBehavior {
  this: FlatSpec =>

  def aScheduler(scheduler: Scheduler) = {
    /*it must "generate an empty schedule when a set of tasks is empty" in {
      if (scheduler.tasks.isEmpty)
        assert(scheduler.schedule().isEmpty)
    }

    it must "generate non-empty schedule when a set of tasks is not empty" in {
      if (scheduler.tasks.nonEmpty)
        assert(scheduler.schedule().nonEmpty)
    }

    it must "produce UnschedulableSetException when utilization of tasks exceeds the amount of cores" in {
      val totalUtilization = scheduler.tasks.map(_.utilization).sum
      if (totalUtilization > scheduler.cores.size) {
        intercept[UnschedulableSetException] {
          scheduler.schedule()
        }
      }
    }*/
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
        val flatSchedule = schedule.flatten.take(task.period).toList.map(_.job)
        val execution = flatSchedule.takeWhile(_ != IdleJob).size
        assert(execution == task.execution)

        val idleTime = flatSchedule.drop(execution).takeWhile(_ == IdleJob).size
        assert(idleTime == task.period - task.execution)
      }
    }
  }



/*  def aSchedulerWithTwoTasksOnOneCore(scheduler: Scheduler, partition: Seq[Set[Task]]): Unit = {
    val tasks = partition.flatten
    it should "contain two tasks" in {
      assert(tasks.size == 2)
    }
    
    it should "have one core" in {
      assert(partition.size == 1)
    }

    it must "have no idle time until first jobs of the tasks are completed" in {
      val sortedTasks = tasks.toList.sortWith(_.period < _.period)
      val taskA = sortedTasks(0)
      val taskB = sortedTasks(1)

      val s = scheduler.schedule(partition).flatten.take(taskA.execution + taskB.execution).toList
      assert(s.count(_ == IdleJob) == 0)
    }
  }*/

  /*
  def aSchedulerWithTwoBigTasksOnTwoCores(scheduler: => Scheduler): Unit = {
    it should "contain two tasks" in {
      assert(scheduler.tasks.size == 2)
    }

    it should "have two cores" in {
      assert(scheduler.cores.size == 2)
    }    
    
    it should "have total utilization > 1" in {
      assert(scheduler.tasks.map(_.utilization).sum > 1)
    }

    it should "schedule one task on one core, another task on the second core" in {
      val limit = scheduler.tasks.map(_.execution).min
      val schedule = scheduler.schedule().take(limit).toList
      val tasks = scheduler.tasks.toSeq

      val taskA = tasks(0)
      val taskB = tasks(1)
      val coreA = scheduler.cores(0)
      val coreB = scheduler.cores(1)

      val jobsCoreA = schedule collect {case map => map(coreA)}
      val jobsCoreB = schedule collect {case map => map(coreB)}

    }
  }
*/
}
