package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical._
import org.kokho.scheduling.{Schedule, ScheduleAnalyzer}

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
class Experiment(val generator: TaskGenerator,
                 val utilizationBound: Double,
                 val cores: Int) {

  val partition = 0.until(cores).map(_ => generateTaskset())


  def run() = {
    val scheduleLocal = new ScheduleWithLocalER(partition)
    val scheduleGlobal = new ScheduleWithGlobalER(partition)
    val scheduleSwap = new SwapSchedule(partition)

    println("Swap   Local   Global")
    List(scheduleSwap, scheduleLocal, scheduleGlobal).foreach(statistics)

  }

  def statistics(sch: Schedule) = {
    val analyzer = new ScheduleAnalyzer(sch, 10000, true)
    analyzer.findMigratedJobs().foreach(println)
    print(analyzer.totalIdleTime)
    print(" ")
  }


  private def toBaselineTaskset(seq: Seq[MulticriticalTask]) =
    seq.map({
      case loTask: LoCriticalTask => LoCriticalTask(loTask.period, loTask.execution, List())
      case hiTask: HiCriticalTask => hiTask
    })


  private def generateTaskset(): Seq[MulticriticalTask] = {
    def helper(acc: List[MulticriticalTask]): List[MulticriticalTask] = {
      val u = acc.map(_.utilization).sum
      if (u < utilizationBound - 0.03)
        helper(generator.generateMulticriticalTask() :: acc)
      else if (u < utilizationBound)
        acc
      else
        helper(acc.tail)
    }

    helper(List())
  }

}
