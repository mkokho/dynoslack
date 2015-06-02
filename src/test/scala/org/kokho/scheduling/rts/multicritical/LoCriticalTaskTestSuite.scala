package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, PeriodicTaskBehavior, TaskBehavior}
import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTaskTestSuite extends FlatSpec with TaskBehavior with PeriodicTaskBehavior{

  def loCriticalTask = LoCriticalTask(10, 4, List(6,8))

  "A low critical task" should behave like aTask(loCriticalTask)

  it should "correctly release early jobs" in {
    def testEarlyReleases(task: LoCriticalTask): Unit = {
      assert(task.earlyReleases.nonEmpty, "Cannot test a task with no early releases")

      val step = task.earlyReleases.head
      val limit = task.period * (1 + task.period / step )

      var currentTask = task
      for (t <- 0.until(limit, step)) {
        val job  = currentTask.jobs(t).next()
        assert(job.release == t)
        currentTask = currentTask.releaseEarlyJob(t + step)
      }
    }

    testEarlyReleases(LoCriticalTask(5, 1, List(2)))
    testEarlyReleases(LoCriticalTask(5, 2, List(4)))
  }

  it should "not release early jobs if time % period == 0 " in {
    def testPosiibilityOfEarlyReleases (loTask: LoCriticalTask): Unit = {
      assert(!loCriticalTask.canReleaseEarlyJob(0))
      assert(!loCriticalTask.canReleaseEarlyJob(loTask.period))
      assert(!loCriticalTask.canReleaseEarlyJob(loTask.execution - 1))

      assert(loTask.earlyReleases.nonEmpty)
      val time = loTask.earlyReleases.head

      assert(loCriticalTask.canReleaseEarlyJob(time))
      assert(loCriticalTask.canReleaseEarlyJob(time + loTask.period))
      val nextLoTask = loTask.releaseEarlyJob(time)

      assert(!nextLoTask.canReleaseEarlyJob(time))
      assert(!nextLoTask.canReleaseEarlyJob(time + loCriticalTask.period))

    }

    testPosiibilityOfEarlyReleases(loCriticalTask)
  }

}
