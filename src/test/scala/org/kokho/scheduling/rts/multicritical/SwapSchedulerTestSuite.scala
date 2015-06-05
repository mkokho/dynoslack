package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
abstract class SwapSchedulerTestSuite extends FlatSpec with SchedulerBehavior with ScheduleBehavior {

  val swapScheduler: Scheduler
  val schedule: Schedule
/*

  "A schedule of a SwapScheduler" should behave like aSchedule(swapScheduler.schedule(twoTasksOneCorePartition))



  "A SwapScheduler of one task on one core" should
    behave like
    aSchedulerWithOneTaskOnOneCore(swapScheduler.schedule(oneTaskOneCorePartition), oneTaskOneCorePartition)



  "A SwapScheduler" should "be able to execute jobs in low-critical mode" in {
    val task = HiCriticalTask(10, 4, 6, isOdd)
    val schedule = swapScheduler.schedule(Set(task), 1).flatten.take(20).toList.map(_.job)

    var execution: Int = schedule.takeWhile(_ != IdleJob).size
    assert(execution == task.hiExecution)

    execution = schedule.drop(task.period).takeWhile(_ != IdleJob).size
    assert(execution == task.loExecution)

  }
*/



}


/*


  val loSet = Set[LoCriticalTask]() +
    LoCriticalTask(10, 4, List(6, 8))

  val hiSet = Set[HiCriticalTask]() +
    HiCriticalTask(10, 4, 6)

  "A SwapScheduler" must "produce a schedule" in {
    //    s.take(10) foreach println
    //    Scheduler.printSchedule(s)
  }

  def isOdd(x: Int) = x % 2 == 1

  def oneTaskOneCorePartition: Seq[Set[MulticriticalTask]] = {
    val task = HiCriticalTask(10, 4, 6)
    Seq(Set(task))
  }

  def twoTasksOneCorePartition: Seq[Set[MulticriticalTask]] = {
    val taskA = HiCriticalTask(8, 4, 4)
    val taskB = HiCriticalTask(12, 4, 4)
    Seq(Set(taskA, taskB))
  }


 "produce valid schedules" in {
    aValidSchedule(swapScheduler.schedule(oneTaskOneCorePartition))
    aValidSchedule(swapScheduler.schedule(twoTasksOneCorePartition))

    val tasks = Set[MulticriticalTask]() +
      HiCriticalTask(8,4,6, isOdd) +
      HiCriticalTask(24, 1, 3) +
      HiCriticalTask(4, 1, 3)

    aValidSchedule(swapScheduler.schedule(tasks, 2))
  }*/
