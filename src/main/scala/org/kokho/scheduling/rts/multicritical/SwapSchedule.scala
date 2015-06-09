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

  private val schedulesPermutation: mutable.Map[LocalSchedule, LocalSchedule] =
    mutable.Map() ++ localSchedules.map(sch => sch -> sch)


  private def releaseGlobally(task: LoCriticalTask, localSchedule: LocalSchedule) = {
    println(task)
    println(localSchedule)
    println("No global release yet")
  }


  private def allSchedulePairs() = schedulesPermutation.keysIterator.toList.combinations(2)

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
            if (a.slackBehind + b.slackAhead >= task.execution) Some(SwapPoint(t, startSchedule, endSchedule))
            else if (b.slackBehind + a.slackAhead >= task.execution) Some(SwapPoint(t, endSchedule, startSchedule))
            else findHelper(states, t + 1)
          case _ => throw new IllegalArgumentException(s"Unxpected variable: $states")
        }
      }
    }

    findHelper(states, absoluteTime + 1)
  }

  def doSwap(task: LoCriticalTask, swapPoint: SwapPoint): Unit = {
    swapPoints += swapPoint
    val job = swapPoint.startSchedule.releaseEarlyJob(task)
    println("releasing early job: "+ job)
  }

  private def releaseSwap(task: LoCriticalTask) = {
    def releaseSwapHelper(pairs: List[Seq[LocalSchedule]]): Unit = pairs match {
      case Nil =>
      case head :: tail =>
        findSwapPoint(head(0), head(1), task) match {
          case None => releaseSwapHelper(tail)
          case Some(sp) => doSwap(task, sp)
        }
    }

    releaseSwapHelper(allSchedulePairs().toList)
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


  def swapSchedules(): Unit = if (swapPoints.nonEmpty) {
    val swapPoint = swapPoints.head
    if (swapPoint.t == absoluteTime) {
      /*before:
        swapPoint.startSchedule -> schA
        swapPoint.endSchedule -> schB
      */
      val schA = schedulesPermutation(swapPoint.startSchedule)
      val schB = schedulesPermutation(swapPoint.endSchedule)

      /* after:
        swapPoint.startSchedule -> schB
        swapPoint.endSchedule -> schA
      */
      schedulesPermutation.update(swapPoint.startSchedule, schB)
      schedulesPermutation.update(swapPoint.endSchedule, schA)
      //

//      val job = schA.extractSwapJob()
//      schB.insertSwapJob(job)

      //drop swap point
    }
  }

  override def next(): Seq[ScheduledJob] = {
//    swapSchedules()
//    slackReclamation()

    absoluteTime += 1
    localSchedules.map(itr => itr.next())
  }

  private case class SwapPoint(t: Int, startSchedule: LocalSchedule, endSchedule: LocalSchedule)


}
