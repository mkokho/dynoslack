package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.multicritical.system.{HiCriticalTask, LoCriticalTask}
import org.kokho.scheduling.{SchedulerAnalyzer, Job, ScheduledJob}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 6/7/2015.
 * @author: Mikhail Kokho
  */
class SchedulerWithGlobalER_TestSuite extends FlatSpec
with Matchers
with MulticriticalSchedulerBehavior {

  override def toScheduler(p: Partition): MulticriticalScheduler = new SchedulerWithGlobalER(p)

  def simple2coreSet = (
    Seq() :+
      HiCriticalTask("A", 4, 2, 2) :+
      LoCriticalTask(6, 2, List(2, 4)),
    Seq() :+
      HiCriticalTask("B", 6, 4, 4)
    )

  behavior of "A global ER schedule"

  it should behave like aMulticriticalScheduler(simple2coreSet)

  it should "release jobs globally" in {
    val analyzer = new SchedulerAnalyzer(simple2coreSet, 6)
    val loTask = analyzer.scheduler.tasks.collectFirst({ case t: LoCriticalTask => t}).get

    val scheduleOfJobs: Seq[ScheduledJob] = analyzer.findJobs(loTask)
    val foundJobs: Seq[Job] = scheduleOfJobs.map(_.scheduledJob)
    val erJobAt2 = loTask.shift(2).job(0)
    val erJobAt4 = loTask.shift(4).job(0)

    foundJobs should not contain erJobAt2
    foundJobs should contain(erJobAt4)
  }


  it should "not release an early job twice" in {
    val taskA = HiCriticalTask("A", 4, 2, 2)
    val taskB = HiCriticalTask("B", 4, 3, 3)
    val taskL = LoCriticalTask(4, 1, List(3))
    val taskSet = (Seq(taskA, taskL), Seq(taskB))
    val analyzer = new SchedulerAnalyzer(taskSet, 4)

    val releasedJobs = analyzer.findJobs(taskL)
    assert(releasedJobs.size == 2)
  }

  it should "not release an early job if the task is active on another processor" in {
    val taskA = HiCriticalTask("A", 14, 8, 8)
    val taskB = HiCriticalTask("B", 8, 5, 5)
    val taskC = HiCriticalTask("C", 9, 6, 6)
    val taskL = LoCriticalTask(7, 3, List(4))

    val partition = Seq(Seq(taskA, taskL), Seq(taskB), Seq(taskC))
    val analyzer = new SchedulerAnalyzer(partition, 15)

    val toJobs: Seq[Job] = analyzer.taskToJobs(taskL).toList
    toJobs should contain (taskL.shift(4).job(0))

    val overlappedJobs = analyzer.findDoubleReleases()
    assert(overlappedJobs.isEmpty, s"There is simultaneous release of jobs: $overlappedJobs ")
  }

}
