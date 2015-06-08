package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{IdleJob, Job}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */
class LocalER_ScheduleTestSuite extends FlatSpec
with Matchers
with MulticriticalScheduleBehavior {

  override def toSchedule(p: Partition): MulticriticalSchedule = new ScheduleWithLocalER(p)

  private def testEarlyJobRelease(task: LoCriticalTask): Unit = {
    val jobs = toSchedule(Seq(Seq(task))).flatten
    val earlyRelease = task.earlyReleases.head

    0.until(10) foreach { _ =>
      val exec = jobs.take(task.execution).toList
      exec.count(_.job != IdleJob) shouldEqual task.execution
      jobs.drop(earlyRelease - task.execution)
    }
  }

  private def checkEarlyReleaseInThe2ndPeriod(tasks: Seq[MulticriticalTask]): Unit = {
    assert(tasks.size == 2)
    val hiTask = tasks(0) match {
      case t: HiCriticalTask => t
    }
    val loTask = tasks(1) match {
      case t: LoCriticalTask => t
    }

    var schedule = toSchedule(Seq(tasks)).toStream.flatten

    assert(hiTask.period == loTask.period)
    val period = hiTask.period

    val firstJobs = Set(hiTask.job(0), loTask.job(0))
    val firstScheduledJobs = schedule.take(period).map(_.job).filter(_ != IdleJob).toSet

    assert(firstJobs == firstScheduledJobs, "First jobs of the tasks has not been scheduled correctly")

    schedule = schedule.drop(period)
    val loTaskER = loTask.shiftedTasks(period + loTask.earlyReleases.head)
    val jobs2ndPeriod = Set(hiTask.job(1), loTask.job(1), loTaskER.job(0))
    val scheduled2ndPeriod = schedule.take(period).map(_.job).filter(_ != IdleJob).toSet

    assert(jobs2ndPeriod == scheduled2ndPeriod, "Early released job has not been scheduled")
  }


  def noEarlyReleaseSet = Seq() :+
    HiCriticalTask(6, 2, 3) :+
    LoCriticalTask(6, 2, List()) :+
    LoCriticalTask(18, 3, List())

  def noEarlyRelease2coreSet = (
    Seq() :+
      HiCriticalTask(7, 2, 3) :+
      LoCriticalTask(11, 2, List()),
    Seq() :+
      HiCriticalTask(7, 2, 3) :+
      LoCriticalTask(11, 5, List()) :+
      LoCriticalTask(19, 2, List())
    )

  def onlyOneLoTask = Seq() :+
    LoCriticalTask(6, 2, List(3))

  def oneLTaskWithEnoughDemand = Seq() :+
    LoCriticalTask(6, 4, List(5))

  behavior of "A local ER schedule (when no LO task has early release points) on one core"

  it should behave like aMulticriticalSchedule(noEarlyReleaseSet)

  behavior of "A local ER schedule (when no LO task has early release points) on two cores"

  it should behave like aMulticriticalSchedule(noEarlyRelease2coreSet)

  behavior of "A local ER schedule (when LO task is the only task " +
    "and there is enough static slack for early releases in each period)"

  it should behave like aMulticriticalSchedule(onlyOneLoTask)

  it should "have only one task and enough static slack" in {
    onlyOneLoTask.size shouldEqual 1
    val task = onlyOneLoTask.head
    val slack = task.period - task.execution
    slack should be > task.period - task.earlyReleases.head
  }

  it should "release release early jobs" in {
    testEarlyJobRelease(onlyOneLoTask.head)
  }

  behavior of "A local ER schedule (when not enough static slack for whole execution, " +
    "but enough slack for demanded execution)"

  it should behave like aMulticriticalSchedule(oneLTaskWithEnoughDemand)

  it should "release early jobs" in {
    testEarlyJobRelease(oneLTaskWithEnoughDemand.head)
  }

  behavior of "A local ER schedule (when available slack changes at runtime, no static slack)"

  def twoTasksNoStaticSlack = Seq() :+
    HiCriticalTask(6, 2, 4, isOdd) :+
    LoCriticalTask(6, 2, List(4))

  it should behave like aMulticriticalSchedule(twoTasksNoStaticSlack)

  it should "not release job in the first period, and release job in the second period" in {
    checkEarlyReleaseInThe2ndPeriod(twoTasksNoStaticSlack)
  }

  behavior of "A local ER schedule (when available slack changes at runtime, with static slack)"

  def twoTasksWithStaticSlack = Seq() :+
    HiCriticalTask(7, 3, 4, isOdd) :+
    LoCriticalTask(7, 2, List(5))

  it should behave like aMulticriticalSchedule(twoTasksWithStaticSlack)

  it should "not release job in the first period, and release job in the second period" in {
    checkEarlyReleaseInThe2ndPeriod(twoTasksWithStaticSlack)
  }

  behavior of "A local ER schedule (with two LO tasks, and enough dynamic slack for two releases)"

  def simultaneousRelease = Seq() :+
    HiCriticalTask(10, 2, 6, isOdd) :+
    LoCriticalTask(10, 2, List(6)) :+
    LoCriticalTask(10, 2, List(6))

  it should behave like aMulticriticalSchedule(simultaneousRelease)

  it should "release two jobs at the same time" in {
    val tasks = simultaneousRelease
    val hiTask = tasks(0)
    val loTaskA = tasks(1).asInstanceOf[LoCriticalTask]
    val loTaskB = tasks(2).asInstanceOf[LoCriticalTask]

    val schedule = toSchedule(Seq(tasks)).flatten

    //the first 8 units must be busy
    val firstJobs: Seq[Job] = Seq(hiTask.job(0), loTaskA.job(0), loTaskB.job(0))
    val firstScheduledJobs:Seq[Job] = schedule.take(10).map(_.job).toSet.toSeq

    assert(firstJobs.intersect(firstScheduledJobs).size == 3,
      "First jobs of the tasks has not been scheduled correctly")

    val loTaskA_ER = loTaskA.shiftedTasks(16)
    val loTaskB_ER = loTaskB.shiftedTasks(16)
    val jobs2ndPeriod = Set(hiTask.job(1),
      loTaskA.job(1), loTaskB.job(1),
      loTaskA_ER.job(0), loTaskB_ER.job(0))
    val scheduled2ndPeriod = schedule.take(10).map(_.job).toSet

    assert(jobs2ndPeriod == scheduled2ndPeriod, "Early released job has not been scheduled")
  }
}
