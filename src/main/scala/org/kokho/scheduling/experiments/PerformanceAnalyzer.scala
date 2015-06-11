package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical._
import org.kokho.scheduling.{Schedule, ScheduleAnalyzer}

import scala.collection.mutable

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
object PerformanceAnalyzer extends App{

  private def toBaselineTaskset(seq: Seq[MulticriticalTask]) =
    seq.map({
      case loTask: LoCriticalTask => LoCriticalTask(loTask.period, loTask.execution, List())
      case hiTask: HiCriticalTask => hiTask
    })

  def baselineSchedule(partition: Seq[Seq[MulticriticalTask]]) = partition.map(toBaselineTaskset)

  def generateTasks(size: Int) = {
    val gen = new TaskGenerator(0.8)
    0.until(size).map(_ => gen.generateMulticriticalTask())
  }

  private def generateTaskset(uBound: Double): Seq[MulticriticalTask] = {
    val gen = new TaskGenerator(0.8)

    val builder = mutable.Set.empty[MulticriticalTask]
    var setUtilization:Double = 0
    while(setUtilization < uBound - 0.05){
      val task = gen.generateMulticriticalTask()
      if (task.utilization + setUtilization <= uBound){
        builder += task
        setUtilization += task.utilization
      }
    }

    Seq() ++ builder
  }

  def generateTaskSet(cores: Int): Seq[Seq[MulticriticalTask]] ={
    def helper(uBound: Double, amount: Int): List[Seq[MulticriticalTask]] = amount match {
      case 0 => Nil
      case _ => generateTaskset(uBound) :: helper(uBound, amount - 1)
    }

    helper(0.8, cores)
  }

  def statistics(sch: Schedule) = {
    val analyzer = new ScheduleAnalyzer(sch, 10000, true)
    println(analyzer.totalIdleTime)
  }

  def run(): Unit = {
    val taskset = generateTaskSet(16)

    val scheduleLocal = new ScheduleWithLocalER(taskset)
    val scheduleGlobal = new ScheduleWithGlobalER(taskset)
    val scheduleSwap = new SwapSchedule(taskset)

    List(scheduleSwap, scheduleLocal, scheduleGlobal).foreach(statistics)
  }

  run()
}
