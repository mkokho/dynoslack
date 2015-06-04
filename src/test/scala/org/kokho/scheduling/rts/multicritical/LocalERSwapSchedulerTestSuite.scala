package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{IdleJob, Job}
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */
class LocalERSwapSchedulerTestSuite extends FunSuite{

  val swapScheduler = new SwapScheduler()

  val isOdd: Int => Boolean = (x:Int) => x % 2 == 1

  private def checkEarlyReleaseInThe2ndPeriod(tasks: Seq[MulticriticalTask]): Unit = {
    assert(tasks.size == 2)
    val hiTask = tasks(0) match {case t: HiCriticalTask => t}
    val loTask = tasks(1) match {case t: LoCriticalTask => t}

    val schedule = swapScheduler.schedule(Seq(Set(hiTask, loTask))).flatten

    assert(hiTask.period == loTask.period)
    val period = hiTask.period

    val firstJobs = Set(hiTask.job(0), loTask.job(0))
    val firstScheduledJobs = schedule.take(period).map(_.job).filter(_ != IdleJob).toSet

    assert(firstJobs == firstScheduledJobs, "First jobs of the tasks has not been scheduled correctly")

    val loTaskER = loTask.shiftedTasks(period + loTask.earlyReleases.head)
    val jobs2ndPeriod = Set(hiTask.job(1), loTask.job(1), loTaskER.job(0))
    val scheduled2ndPeriod = schedule.take(period).map(_.job).filter(_ != IdleJob).toSet

    assert(jobs2ndPeriod == scheduled2ndPeriod, "Early released job has not been scheduled")

  }

  test("testing local early release without static slack") {
    val hiTask = HiCriticalTask(6, 2, 4, isOdd)
    val loTask = LoCriticalTask(6, 2, List(4))

    checkEarlyReleaseInThe2ndPeriod(Seq(hiTask, loTask))
  }

  test("testing local early release with static slack") {
    val hiTask = HiCriticalTask(7, 3, 4, isOdd)
    val loTask = LoCriticalTask(7, 2, List(5))

    checkEarlyReleaseInThe2ndPeriod(Seq(hiTask, loTask))
  }

  test("testing local early release of two jobs at the same time") {
    val hiTask = HiCriticalTask(10, 2, 6, isOdd)
    val loTaskA = LoCriticalTask(10, 2, List(4))
    val loTaskB = LoCriticalTask(10, 2, List(4))

    val schedule = swapScheduler.schedule(Seq(Set(hiTask, loTaskA, loTaskB))).flatten

    //the first 8 units must be busy
    val firstJobs:Set[Job] = Set(hiTask.job(0), loTaskA.job(0), loTaskB.job(0))
    val firstScheduledJobs = schedule.take(10).map(_.job).toSet

    assert(firstJobs == firstScheduledJobs, "First jobs of the tasks has not been scheduled correctly")

    val loTaskA_ER = loTaskA.shiftedTasks(14)
    val loTaskB_ER = loTaskB.shiftedTasks(14)
    val jobs2ndPeriod = Set(hiTask.job(1), loTaskA.job(1), loTaskB.job(1), loTaskA_ER.job(0), loTaskB_ER.job(0))
    val scheduled2ndPeriod = schedule.take(10).map(_.job).toSet

    assert(jobs2ndPeriod == scheduled2ndPeriod, "Early released job has not been scheduled")
  }

}
