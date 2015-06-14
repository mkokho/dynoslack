package org.kokho.scheduling.experiments

import org.kokho.scheduling.ScheduleAnalyzer
import org.kokho.scheduling.rts.multicritical._
import org.slf4j.LoggerFactory

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
class ScheduleComparison(val taskset: Seq[Seq[MulticriticalTask]], val duration: Int) {

  private val loTasks = taskset.flatten collect { case loTask: LoCriticalTask => loTask}
  private val globalERAnalyzer = new ScheduleAnalyzer(new ScheduleWithGlobalER(taskset), duration, true)
  private val swapAnalyzer = new ScheduleAnalyzer(new SwapSchedule(taskset), duration, true)

  private val baselineTaskset = taskset.map(toBaselineTaskset)
  private val baselineAnalyzer = new ScheduleAnalyzer(new ScheduleWithLocalER(baselineTaskset), duration, true)


  private val logger = LoggerFactory.getLogger(classOf[ScheduleComparison])

  def idleDiff() {
    var mistake = 0
    for (t <- 0 until duration) {
      val sIdle = swapAnalyzer.totalIdleTimeBefore(t)
      val gIdle = globalERAnalyzer.totalIdleTimeBefore(t)
      if (sIdle - gIdle - mistake != 0) {
        mistake = sIdle - gIdle
        logger.info(s"Swap: total idle time before $t is $sIdle")
        logger.info(s"G-ER: total idle time before $t is $gIdle")
      }
    }
  }

  def jobsProducedDifference() = {
    for (task <- taskset.flatten) {
      val jobsInSwap = swapAnalyzer.findJobs(task)
      val jobsInGer = globalERAnalyzer.findJobs(task)
      logger.info(s"Task $task:")

      if (task.isInstanceOf[LoCriticalTask]){
        val loTask = task.asInstanceOf[LoCriticalTask]
        val swapReleases = swapAnalyzer.numberOfEarlyReleases(loTask)
        val globalReleases = globalERAnalyzer.numberOfEarlyReleases(loTask)

        logger.info("  released in swap: " + swapReleases)
        logger.info("  released in g-er: " + globalReleases)
      }

      logger.info("  Swap / GlobalER: " + jobsInSwap.diff(jobsInGer))
      logger.info("  GlobalER / Swap: " + jobsInGer.diff(jobsInSwap))
    }
  }

  lazy val frequencyImprovementGlobalER = frequencyImprovement(globalERAnalyzer)

  lazy val frequencyImprovementSwap = frequencyImprovement(swapAnalyzer)

  def idleTimeGlobalER = globalERAnalyzer.totalIdleTime

  def idleTimeSwap = swapAnalyzer.totalIdleTime

  def idelTimeNoRelease = baselineAnalyzer.totalIdleTime

  private def getImprovement(analyzer: ScheduleAnalyzer, task: LoCriticalTask): Double =
    analyzer.taskToJobs.get(task) match {
      case None => 1
      case Some(jobs) =>
        val baseFrequency: Double = duration.toDouble / task.period
        val runtimeFrequency: Double = jobs.size.toDouble
        Math.max(1, runtimeFrequency / baseFrequency)
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


  private def toBaselineTaskset(seq: Seq[MulticriticalTask]) =
    seq.map({
      case loTask: LoCriticalTask => LoCriticalTask(loTask.period, loTask.execution, List())
      case hiTask: HiCriticalTask => hiTask
    })
}
