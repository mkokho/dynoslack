package org.kokho.scheduling_new.multicritical.system

import org.kokho.scheduling_new.{PeriodicTask, PeriodicTaskBehavior, TaskBehavior}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */
trait LoCriticalTaskBehavior extends FlatSpec
with Matchers
with TaskBehavior
with PeriodicTaskBehavior {
  this: FlatSpec =>

  def aLoCriticalTask(task: LoCriticalTask): Unit = {

    it should behave like aTask(task)

    it should behave like aPeriodicTask(task.asInstanceOf[PeriodicTask])

    it should "release jobs that are different from the jobs of the task with equal parameters" in {
      val copyTask = LoCriticalTask(task.period, task.execution, task.earlyReleases)

      for (idx <- 0.until(10)) {
        assert(task.job(idx) != copyTask.job(idx))
      }
    }

  }

  def aLoCriticalTaskWithEarlyReleases(task: LoCriticalTask) = {

    it should "have early release times" in {
      task.earlyReleases should not be empty
    }

    it should "have early release times between its execution and its period" in {
      for (release <- task.earlyReleases) {
        release should be < task.period
        release should be >= task.execution
      }
    }

    it should "create child tasks that release early jobs" in {
      for {
        idx <- 0.to(2)
        relativeRelease <- task.earlyReleases
        release = task.offset + idx*task.period + relativeRelease
      } {
        assert(task.canReleaseEarlyJob(release))
      }
    }

    it should "correctly release early jobs" in {
      val step = task.earlyReleases.head
      val limit = task.offset + task.period * (1 + task.period / step)

      var currentTask = task
      for (t <- task.offset.until(limit, step)) {
        val job = currentTask.jobs(t).next()
        assert(job.release == t)
        currentTask = currentTask.shift(t + step)
      }
    }

    it should "create equal children tasks that release equal jobs" in {
      val childA = childTask(task)
      val childB = childTask(task)

      assert(childA == childB, "Children are not equal")

      for (idx <- 0.to(20)){
        assert(childA.job(idx) == childB.job(idx))
      }
    }
  }

  def childTask(task: LoCriticalTask): LoCriticalTask = {
    val release = task.earlyReleases.head + task.period + task.offset
    task.shift(release)
  }
}
