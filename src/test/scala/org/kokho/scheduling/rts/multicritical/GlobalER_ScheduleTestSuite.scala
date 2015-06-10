package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, ScheduleAnalyzer, ScheduledJob}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 6/7/2015.
 * @author: Mikhail Kokho
  */
class GlobalER_ScheduleTestSuite extends FlatSpec
with Matchers
with MulticriticalScheduleBehavior {

  override def toSchedule(p: Partition): MulticriticalSchedule = new ScheduleWithGlobalER(p)

  def simple2coreSet = (
    Seq() :+
      HiCriticalTask("A", 4, 2, 2) :+
      LoCriticalTask(6, 2, List(2, 4)),
    Seq() :+
      HiCriticalTask("B", 6, 4, 4)
    )

  behavior of "A global ER schedule"

  it should behave like aMulticriticalSchedule(simple2coreSet)

  it should "release jobs globally" in {
    val analyzer = new ScheduleAnalyzer(simple2coreSet, 6)
    val loTask = analyzer.schedule.tasks.collectFirst({ case t: LoCriticalTask => t}).get

    val scheduleOfJobs: Seq[ScheduledJob] = analyzer.findJobs(loTask)
    val foundJobs: Seq[Job] = scheduleOfJobs.map(_.job)
    val erJobAt2 = loTask.shiftedTasks(2).job(0)
    val erJobAt4 = loTask.shiftedTasks(4).job(0)

    foundJobs should not contain erJobAt2
    foundJobs should contain(erJobAt4)
  }


  it should "not release an early job twice" in {
    val taskA = HiCriticalTask("A", 4, 2, 2)
    val taskB = HiCriticalTask("B", 4, 3, 3)
    val taskL = LoCriticalTask(4, 1, List(3))
    val taskSet = (Seq(taskA, taskL), Seq(taskB))
    val analyzer = new ScheduleAnalyzer(taskSet, 4)

    val releasedJobs = analyzer.findJobs(taskL)
    assert(releasedJobs.size == 2)
  }

}
