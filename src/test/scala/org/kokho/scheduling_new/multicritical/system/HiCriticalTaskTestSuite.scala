package org.kokho.scheduling_new.multicritical.system

import org.kokho.scheduling_new.{PeriodicTask, PeriodicTaskBehavior, TaskBehavior}
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class HiCriticalTaskTestSuite extends FlatSpec with TaskBehavior with PeriodicTaskBehavior{

  def hiCriticalTask = HiCriticalTask(10, 4, 6, isOdd(_))

  def isOdd(x: Int) = x % 2 == 1

  "A high critical task" should behave like aTask(hiCriticalTask)

  it should behave like aPeriodicTask(hiCriticalTask.asInstanceOf[PeriodicTask])

  it should "return jobs that take low WCET" in {
    val jobs = hiCriticalTask.jobs()
    val job0 = jobs.next()
    val job1 = jobs.next()

    assert(job0.length == hiCriticalTask.hiExecution)
    assert(job1.length == hiCriticalTask.loExecution)
  }
}
