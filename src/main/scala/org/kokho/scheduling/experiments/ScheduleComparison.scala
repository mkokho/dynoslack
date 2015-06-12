package org.kokho.scheduling.experiments

import org.kokho.scheduling.ScheduleAnalyzer
import org.kokho.scheduling.rts.multicritical._

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
class ScheduleComparison(val taskset: Seq[Seq[MulticriticalTask]], val duration: Int) {

  private val loTasks = taskset.flatten collect { case loTask: LoCriticalTask => loTask}
  private val globalERAnalyzer = new ScheduleAnalyzer(new ScheduleWithGlobalER(taskset), duration, true)
  private val swapAnalyzer = new ScheduleAnalyzer(new SwapSchedule(taskset), duration, true)


  def frequencyImprovementGlobalER = frequencyImprovement(globalERAnalyzer)

  def frequencyImprovementSwap = frequencyImprovement(swapAnalyzer)

  def idleTimeGlobalER = globalERAnalyzer.totalIdleTime

  def idleTimeSwap = swapAnalyzer.totalIdleTime


  private def getImprovement(analyzer: ScheduleAnalyzer, task: LoCriticalTask): Double =
    analyzer.taskToJobs.get(task) match {
      case None =>
        println("Cannot compute frequency improvement. There is not such task");
        1
      case Some(jobs) =>
        val baseFrequency: Double = duration.toDouble / task.period
        val runtimeFrequency: Double = jobs.size.toDouble
        if (runtimeFrequency < baseFrequency) println("Error: runtime frequency is smaller than base frequency")
        runtimeFrequency / baseFrequency
    }

  private def frequencyImprovement(analyzer: ScheduleAnalyzer): Double = {
    if (loTasks.isEmpty) {
      println("The task set has no low-criticality tasks")
      1
    }
    else {
      val allImprovements: Seq[Double] = loTasks.map(getImprovement(analyzer, _))
      org.kokho.utils.Math.mean(allImprovements)
    }
  }

  //  val baselineAnalyzer = new ScheduleAnalyzer(new ScheduleWithLocalER(baselineTaskset), duration, true)
  //  private val baselineTaskset = taskset.map(toBaselineTaskset)


  private def toBaselineTaskset(seq: Seq[MulticriticalTask]) =
    seq.map({
      case loTask: LoCriticalTask => LoCriticalTask(loTask.period, loTask.execution, List())
      case hiTask: HiCriticalTask => hiTask
    })
}
