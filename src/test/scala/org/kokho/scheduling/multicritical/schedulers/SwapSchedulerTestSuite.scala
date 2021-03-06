package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.multicritical.system.{HiCriticalTask, LoCriticalTask}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapSchedulerTestSuite extends FlatSpec
with Matchers
with MulticriticalSchedulerBehavior {

  val loTask = LoCriticalTask(12, 4, List(5))
  val taskA = HiCriticalTask("A", 12, 1, 3, lowJobs)
  val taskB = HiCriticalTask("B", 17, 7, 7)
  val taskC = HiCriticalTask("C", 9, 7, 7)
  val swapSet = (Seq(taskA, taskB, loTask), Seq(taskC))
  val swap3Set = (
    swapSet._1,
    Seq(HiCriticalTask("C", 9, 7, 7)),
    Seq(HiCriticalTask("D", 9, 7, 7))
    )

  def lowJobs = Set(0)

  override def toScheduler(tasks: Partition): MulticriticalScheduler = new SwapScheduler(tasks)

  behavior of "A Swap Schedule"

  it should behave like aMulticriticalScheduler(swapSet)

  /*
  it should "release jobs locally and globally" in {
    val myLoTask = LoCriticalTask(6, 2, List(2, 4))
    val simple2coreSet = (
      Seq() :+ HiCriticalTask("A", 4, 2, 2) :+ myLoTask,
      Seq() :+ HiCriticalTask("B", 6, 4, 4)
      )

    val analyzer = new SchedulerAnalyzer(simple2coreSet, 6)

    val scheduleOfJobs: Seq[ScheduledJob] = analyzer.findJobs(myLoTask)
    val foundJobs: Seq[Job] = scheduleOfJobs.map(_.scheduledJob)
    val erJobAt2 = myLoTask.shift(2).job(0)
    val erJobAt4 = myLoTask.shift(4).job(0)

    foundJobs should not contain erJobAt2
    foundJobs should contain(erJobAt4)

    validityCheck(new SchedulerAnalyzer(simple2coreSet, 1000))
  }

  it should "permute schedules" in {
    val analyzer = new SchedulerAnalyzer(swapSet, 12)
    val extraJob = loTask.shift(5).job(0)

    //the processor 1 should execute jobs A, E0, E1, C1
    //the processor 2 should execute jobs C0 and B0

    val core1 = analyzer.jobStream(0).map(_.scheduledJob)
    core1 should contain(taskA.job(0))
    core1 should contain(loTask.job(0))
    core1 should contain(extraJob)
    core1 should contain(taskC.job(1))

    val core2 = analyzer.jobStream(1).map(_.scheduledJob)
    core2 should contain(taskC.job(0))
    core2 should contain(taskB.job(0))

    validityCheck(new SchedulerAnalyzer(swapSet, 1000))
  }

  it should "release a swap job" in {
    val analyzer = new SchedulerAnalyzer(swapSet, 12)

    val scheduleOfJobs: Seq[ScheduledJob] = analyzer.findJobs(loTask)
    val foundJobs: Seq[Job] = scheduleOfJobs.map(_.scheduledJob)

    val extraJob = loTask.shift(5).job(0)
    foundJobs should contain(extraJob)

    validityCheck(new SchedulerAnalyzer(swapSet, 1000))
  }

  /**
   * The following set is expected to swap at time 5:
   * A E _ _ _ ~ ~ B _ _ ...
   * C _ _ _ _ _ _ ~ ~ C _ _ _ ...
   * 0   2     5   7   9
   * Idle time is form 5 to 7 on the first core, and from 7 to 9 on the second core
   */

  it should "release swap job only once" in {
    val analyzer = new SchedulerAnalyzer(swap3Set, 12)

    val releasedAmount = analyzer.findJobs(loTask).count(_.to <= 12)

    releasedAmount shouldBe 2

    validityCheck(new SchedulerAnalyzer(swap3Set, 1000))
  }


  it should "swap and release globally" in {
    val swapGlobalSet = (
      Seq() :+
        HiCriticalTask("A", 14, 3, 3) :+
        HiCriticalTask("B", 18, 8, 8) :+
        loTask,
      swapSet._2,
      Seq() :+ HiCriticalTask("C", 7, loExecution = 5, 7, lowJobs)
      )

    //in the task set above, the core 2 is swapped with core 1 at time 5 to accommodate a swap job
    //other jobs of LoTask are executed on the core 0

    val analyzer = new SchedulerAnalyzer(swapGlobalSet, 12)

    val shiftedTask = loTask.shift(5)
    val extraJob = shiftedTask.job(0)
    val nextExtraJob = shiftedTask.job(1)

    analyzer.jobStream(2).map(_.scheduledJob) should contain(extraJob)
    analyzer.jobStream(0).map(_.scheduledJob) should contain(nextExtraJob)

    validityCheck( new SchedulerAnalyzer(swapGlobalSet, 1000))
  }


  it should "correctly reclaim slack for a swap job" in {
    //goal: has two lo critical tasks that can release early simulataneously and available slack for only one early release

    //the first core will contain two LO tasks that will be trying to release early jobs
    //the HI task simply increases utilization of the first core
    val loTaskX = LoCriticalTask("X", 20, 6, List(12))
    val loTaskY = LoCriticalTask("Y", 20, 4, List(12))
    val hiTaskA = HiCriticalTask("A", 40, 20, 20)

    //the second and the third core will have combined slack for execution of only one early release job
    //the third core has enough slack for execution of a shorter low critical job
    //this slack should be claimed by release of longer critical job
    //also we make sure that a swap job is not continually executed
    //when swapped it will wait for completion of the local job
    val taskB = HiCriticalTask("B", 14, 3, 5, Set(0))
    val taskC = HiCriticalTask("C", 14, 9, 9)

    //taskD creates slack at runtime which we should not use because swap plan will break
    val taskD = HiCriticalTask("D", 20, 13, 14, Set(0))
    val taskE = HiCriticalTask("E", 21, 2, 2)

    /*schedule expectation (swap fixed at the time 10)
    *
    *   ~ is  slack
    *   ^ is unpredicted slack (when planning swap schedule)
    *   _ is executions of a job
    *
    * X _ _ _ _ Y _ _ _ A _ _ _ _ _ _ _ _ _ _ _ _ _ _ ...
    * 1 2   4   6   8 9   1 2 3 4 5 6 7 8 9   1
    *                   10                  20
    * B C _ _ _ _ _ _ _ _ _ ~ ~ B _ _ _ _ C _ _ _ _ _ ...
    * 1 2   4   6   8 9   1 2 3 4 5 6 7 8 9   1
    *                   10                  20
    * D _ _ _ _ _ _ _ _ _ _ _ ^ E _ ~ ~ ~ ~ D _ _ _ _ ...
    * 1 2   4   6   8 9   1 2 3 4 5 6 7 8 9   1
    *                   10                  20
    */

    def doCheck(analyzer: SchedulerAnalyzer) {
      val jobsX = analyzer.findJobs(loTaskX)
      val jobsY = analyzer.findJobs(loTaskY)

      val earlyJobX = loTaskX.shift(12).job(0)
      val earlyJobY = loTaskY.shift(12).job(0)

      //exactly one early job is released
      (jobsY find (_.scheduledJob == earlyJobY), jobsX find (_.scheduledJob == earlyJobX)) match {
        case (None, None) => fail("At least one early job must have been released")
        case (Some(_), Some(_)) => fail("Only one early job must have been released")
        case _ => //good
      }

      validityCheck(analyzer)
    }

    val taskset = (Seq(loTaskX, loTaskY, hiTaskA), Seq(taskB, taskC), Seq(taskD, taskE))
    doCheck(new SchedulerAnalyzer(taskset, 1500))

    val taskSetReverse = (taskset._1.reverse, taskset._2, taskset._3)
    doCheck(new SchedulerAnalyzer(taskSetReverse, 1500))
  }
*/
}

