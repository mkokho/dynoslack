package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduledJob
import org.slf4j.LoggerFactory

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */

final class SwapSchedule(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition) {

  private val logger = LoggerFactory.getLogger(classOf[SwapSchedule])

  private var absoluteTime = 0

  private val swapPoints: mutable.Queue[SwapPoint] = mutable.Queue()

  private val schedulesPermutation: Array[LocalSchedule] = localSchedules.toArray


  private def releaseGlobally(task: LoCriticalTask, destinationSchedule: LocalSchedule) = {
    val hostSchedule = taskToLocalSchedule(task).get
    val job = hostSchedule.releaseEarlyJob(task)
    destinationSchedule.insertJob(job)
  }

  private def findSwapPoint(startSchedule: LocalSchedule,
                            endSchedule: LocalSchedule,
                            task: LoCriticalTask): Option[SwapPoint] = {

    val states = List(startSchedule, endSchedule)
      .map(_.slackForecast(task.deadline).toList)
      .map(new SlackAnalyzer(_, absoluteTime, absoluteTime + task.deadline))

    assert(states.count(_.totalSlack >= task.execution) == 0,
      "Swap is redundant. There is enough slack on one processor")

    def findHelper(states: List[SlackAnalyzer], t: Int): Option[SwapPoint] = {
      if (t - absoluteTime >= task.deadline) None
      else {
        states foreach (_.advanceTime())
        if (!states.forall(_.isSwapAvailable))// || startSchedule.isFutureBusy(t) || endSchedule.isFutureBusy(t))
          findHelper(states, t + 1)
        else states match {
          case a :: b :: _ =>
            if (a.slackBehind + b.slackAhead >= task.execution)
              Some(SwapPoint(t, a.slackUnitsBehind(t) ++ b.slackUnitsAhead(t), startSchedule, endSchedule))
            else if (b.slackBehind + a.slackAhead >= task.execution)
              Some(SwapPoint(t, b.slackUnitsBehind(t) ++ a.slackUnitsAhead(t), endSchedule, startSchedule))
            else findHelper(states, t + 1)
          case _ => throw new IllegalArgumentException(s"Unxpected variable: $states")
        }
      }
    }

    if (states.map(_.totalSlack).sum < task.execution)
      None
    else
      findHelper(states, absoluteTime + 1)
  }

  def planSwap(task: LoCriticalTask, swapPoint: SwapPoint): Unit = {
    assert(swapPoint.executionPlan.forall(_ >= absoluteTime), "Cannot back-schedule")

    logger.info("Planning swap at " + swapPoint.t)

    swapPoints += swapPoint
    val taskSchedule = taskToLocalSchedule(task).get
    val job = taskSchedule.releaseEarlyJob(task).job

    //we create two swap job and insert them in the corresponding schedules
    val planStart = swapPoint.executionPlan.takeWhile(_ < swapPoint.t).toList
    val planEnd = swapPoint.executionPlan.dropWhile(_ < swapPoint.t).take(job.length - planStart.size).toList

    swapPoint.startSchedule.insertSwapJob(SwapJob(job, swapPoint.t, planStart, true))
    swapPoint.endSchedule.insertSwapJob(SwapJob(job, swapPoint.t, planEnd, false))
  }

  private def releaseSwap(task: LoCriticalTask) = {
    def releaseSwapHelper(pairs: List[Seq[LocalSchedule]]): Unit = pairs match {
      case Nil =>
      case head :: tail =>
        findSwapPoint(head(0), head(1), task) match {
          case None => releaseSwapHelper(tail)
          case Some(sp) => planSwap(task, sp)
        }
    }

    val allSchedulePairs = localSchedules.filter(!_.isSwapActive()).combinations(2)

    releaseSwapHelper(allSchedulePairs.toList)
  }

  def swapSchedules(): Unit = if (swapPoints.nonEmpty) {
    val swapPoint = swapPoints.head
    if (swapPoint.t == absoluteTime) {
      val idxOfStartSchedule = schedulesPermutation.indexOf(swapPoint.startSchedule)
      val idxOfEndSchedule = schedulesPermutation.indexOf(swapPoint.endSchedule)
      schedulesPermutation(idxOfStartSchedule) = swapPoint.endSchedule
      schedulesPermutation(idxOfEndSchedule) = swapPoint.startSchedule

      swapPoints.dequeue()
    }
  }


  private def slackReclamation() = {
    val tasksForER = localSchedules
      .map(_.tasksForEarlyRelease).flatten
      .filter(task => localSchedules.forall(!_.isSwapActive(task)))

    for (task <- tasksForER) {
      localSchedules find (_.hasSlackForTask(task)) match {
        case Some(sch) => releaseGlobally(task, sch)
        case None => releaseSwap(task)
      }
    }
  }

  private def debug(from: Int, to: Int) = {
    if (absoluteTime >= from && absoluteTime < to) {
      println(s"Debug info at time $absoluteTime")
      val len = to - from
      localSchedules foreach (sch => {
        val forecast: Seq[SlackPeriod] = sch.slackForecast(len)
        val totalSlack = SlackPeriod.totalSlack(forecast, to)
        println(s"Total slack $totalSlack in $forecast")
      })
      println(s"Swap points: $swapPoints")
      println(" - - - - - ")
    }
  }

  override def next(): Seq[ScheduledJob] = {
    //    debug(396, 416)
    swapSchedules()
    slackReclamation()

    absoluteTime += 1

    //    schedulesPermutation.map(localSchedules(_)).map(itr => itr.next()).toList
    schedulesPermutation.map(itr => itr.next()).toList
  }

  private case class SwapPoint(t: Int, executionPlan: Seq[Int], startSchedule: LocalSchedule, endSchedule: LocalSchedule) {

    def slackBehind = executionPlan.count(_ < t)

    def slackAheand = executionPlan.count(_ >= t)

  }

}
