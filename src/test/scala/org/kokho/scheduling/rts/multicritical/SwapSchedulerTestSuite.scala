package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapSchedulerTestSuite extends FlatSpec
with Matchers
with MulticriticalScheduleBehavior {

  override def toSchedule(tasks: Partition): MulticriticalSchedule = new SwapSchedule(tasks)


  /**
   * The following set is expected to swap at time 5:
   * A E _ _ _ ~ ~ B _ _ ...
   * C _ _ _ _ _ _ ~ ~ C _ _ _ ...
   * 0   2     5   7   9
   * Idle time is form 5 to 7 on the first core, and from 7 to 9 on the second core
   */
  def lowJobs = Set(0)

  val loTask = LoCriticalTask(12, 4, List(5))

  val swapSet = (
    Seq() :+
      HiCriticalTask("A", 12, 1, 3, lowJobs) :+
      HiCriticalTask("B", 17, 7, 7) :+
      loTask
    ,
    Seq() :+
      HiCriticalTask("C", 9, 7, 7)
    )

  behavior of "A Swap Schedule"

  ignore should "release jobs locally and globally" in {
    pending
  }

  it should "swap jobs" in {
    val analyzer = new ScheduleAnalyzer(swapSet, 12)

    val scheduleOfJobs: Seq[ScheduledJob] = analyzer.findJobs(loTask)
    val foundJobs: Seq[Job] = scheduleOfJobs.map(_.job)

    val extraJob = loTask.shiftedTasks(5).job(0)

//    foundJobs should contain(extraJob)
  }

  val swap3Set = (
    swapSet._1,
    swapSet._2,
    swapSet._2
    )

  it should "swap only once with this set" in {

    val analyzer = new ScheduleAnalyzer(swap3Set, 12)

  }

  val swapGlobalSet = (
    Seq() :+
      HiCriticalTask("A", 14, 3, 3) :+
      HiCriticalTask("B", 18, 8, 8) :+
      loTask
    ,
    swapSet._2,
    Seq() :+
      HiCriticalTask("C", 7, loExecution =  5, 7, lowJobs)
    )


  it should "swap and release globally" in {
    val analyzer = new ScheduleAnalyzer(swapGlobalSet, 12)
//    analyzer.printSchedule()

  }

}

