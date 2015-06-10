package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduledJob

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */

final class SwapSchedule(partition: Seq[Seq[MulticriticalTask]])
  extends MulticriticalSchedule(partition) {

  private var absoluteTime = 0

  private val swapPoints: mutable.Queue[SwapPoint] = mutable.Queue()

  private val schedulesPermutation: Array[Int] = 0.until(arity).toArray


  private def releaseGlobally(task: LoCriticalTask, destinationSchedule: LocalSchedule) = {
    val hostSchedule = taskToLocalSchedule(task).get
    val job = hostSchedule.releaseEarlyJob(task)
    destinationSchedule.insertJob(job)
  }

  private def allSchedulePairs() = localSchedules.combinations(2)

  private def findSwapPoint(startSchedule: LocalSchedule,
                            endSchedule: LocalSchedule,
                            task: LoCriticalTask): Option[SwapPoint] = {

    val states = List(startSchedule, endSchedule)
      .map(_.slackForecast(task.deadline))
      .map(new SlackAnalyzer(_, absoluteTime, absoluteTime + task.deadline))

    assert(states.count(_.totalSlack >= task.execution) == 0,
      "Swap is redundant. There is enough slack on one processor")

    def findHelper(states: List[SlackAnalyzer], t: Int): Option[SwapPoint] = {
      if (t - absoluteTime >= task.deadline) None
      else {
        states foreach (_.advanceTime())
        if (!states.forall(_.isSwapAvailable)) findHelper(states, t + 1)
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

    findHelper(states, absoluteTime + 1)
  }

  def planSwap(task: LoCriticalTask, swapPoint: SwapPoint): Unit = {
    swapPoints += swapPoint
    val taskSchedule = taskToLocalSchedule(task).get
    val job = taskSchedule.releaseEarlyJob(task).job

    //we create two swap job and insert them in the corresponding schedules
    val planStart = swapPoint.executionPlan.takeWhile(_ < swapPoint.t).toList
    val planEnd = swapPoint.executionPlan.dropWhile(_ < swapPoint.t).take(job.length - planStart.size).toList

    swapPoint.startSchedule.insertSwapJob(SwapJob(job, planStart))
    swapPoint.endSchedule.insertSwapJob(SwapJob(job, planEnd))
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

    releaseSwapHelper(allSchedulePairs().toList)
  }

  def swapSchedules(): Unit = if (swapPoints.nonEmpty) {
    val swapPoint = swapPoints.head
    if (swapPoint.t == absoluteTime) {
      val idxOfStartSchedule = localSchedules.indexOf(swapPoint.startSchedule)
      val idxOfEndSchedule = localSchedules.indexOf(swapPoint.endSchedule)
      val processorOfStartSchedule = schedulesPermutation(idxOfStartSchedule)
      val processorOfEndSchedule = schedulesPermutation(idxOfEndSchedule)
      schedulesPermutation(idxOfStartSchedule) = processorOfEndSchedule
      schedulesPermutation(idxOfEndSchedule) = processorOfStartSchedule

//      move swap job between schedules
//      val job = swapPoint.startSchedule.extractSwapJob()
//      swapPoint.endSchedule.insertSwapJob(job)
      //drop first swap point
      swapPoints.dequeue()

    }
  }


  private def slackReclamation() = {
    val tasksForER = localSchedules.map(_.tasksForEarlyRelease).flatten

    for (task <- tasksForER) {
      localSchedules find (_.hasSlackForTask(task)) match {
        case Some(sch) => releaseGlobally(task, sch)
        case None => releaseSwap(task)
      }
    }
  }

  override def next(): Seq[ScheduledJob] = {
    swapSchedules()
    slackReclamation()

    absoluteTime += 1

    schedulesPermutation.map(localSchedules(_)).map(itr => itr.next()).toList
  }

  private case class SwapPoint(t: Int, executionPlan:Seq[Int], startSchedule: LocalSchedule, endSchedule: LocalSchedule) {

    def slackBehind = executionPlan.count(_ < t)

    def slackAheand = executionPlan.count(_ >= t)

  }

}
