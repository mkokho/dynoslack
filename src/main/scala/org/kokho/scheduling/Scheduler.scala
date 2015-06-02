package org.kokho.scheduling

import org.kokho.binpacking.{BinPacker, WeightedObject}
import org.kokho.scheduling.exceptions.UnschedulableSetException
import org.kokho.utils.Math.lcm

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */

/**
 * Computes a schedule for a set of tasks, or throws UnschedulableSetException
 *
 */
trait Scheduler {

  type AllowedTasks <: Task

  private implicit def taskToWeightedObject(task: Task): WeightedObject = new WeightedObject {
    override def weight: Double = task.utilization
  }

  def schedule(partition: Seq[Set[AllowedTasks]]): Schedule

  def schedule(tasks: Set[AllowedTasks], coreNum: Int): Schedule = {
    val bigTasks = tasks.filter(_.utilization > 1)
    if (bigTasks.size > 0) {
      throw new UnschedulableSetException(s"Cannot schedule tasks with utilization > 1: $bigTasks")
    }

    val partition = BinPacker().packObjects(tasks)
    if (partition.size > coreNum)
      throw new UnschedulableSetException(s"Could not partition tasks to $coreNum cores")

    schedule(partition)
  }

}

abstract class Schedule extends Iterator[Seq[ScheduledJob]] {
  override def hasNext: Boolean = true

  /**
   * Partitioning of the tasks into sets such that each set has utilization not greater than 1
   */
  def partition: Seq[Seq[Task]]

  /**
   * Size of the tuples contained by this Schedule
   */
  def arity: Int = partition.size

  /**
   * The tasks of the schedule
   */
  lazy val tasks: Seq[Task] = partition.flatten

  /**
   * Hyper-period of the tasks of the set partition($idx)
   */
  def hyperPeriod(idx: Int): Int = {
    if (idx < 0 || idx >= arity) {
      throw new IllegalArgumentException(
        s"Index is out of bound. Expected between 0 and $arity. Given $idx"
      )
    }

    lcm(partition(idx).map(_.period).iterator)
  }
}

/*
object Scheduler {

 private def convertToScheduledJobs(schedule: Iterator[Job]): Iterator[ScheduledJob] = {
    new Iterator[ScheduledJob] {
      override def hasNext: Boolean = true

      private var from: Int = 0
      private val itr: BufferedIterator[Job] = schedule.buffered

      override def next(): ScheduledJob = {
        val j = itr.head
        var p = 0
        while (j == itr.head) {
          itr.next();
          p += 1
        }
        from = from + p
        ScheduledJob(from - p, from, j)
      }
    }
  }

  private def printSchedule(schedule: Iterator[ScheduledJob], from: Int, to: Int): String = {
    if (to <= from)
      throw new IllegalArgumentException(s"'From' must be smaller than 'to'. Given: $from < $to")


    val allocation = schedule.takeWhile(_.from < to).toList

    val sbJobs = new StringBuilder(2 * to)
    val sbIntervals = new StringBuilder(2 * to)
    val sbTicks = new StringBuilder(2 * to)

    for (a <- allocation) {
      val name = a.job.toString
      sbJobs ++= " " * (a.length - 1)
      sbJobs ++= name
      sbJobs ++= " " * (a.length + 1 - name.length)
    }

    for (a <- allocation) {
      if (a.from == a.job.release)
        sbIntervals ++= "↓_"
      else
        sbIntervals ++= "|_"
      sbIntervals ++= " _" * (a.length - 1)
    }

    for (t <- Range(0, to, 1)) {
      if (t % 10 == 0 && t != 0) {
        if (t >= 100) {
          sbTicks ++= t.toString.drop(1)
        } else {
          sbTicks ++= t.toString
        }
      } else if (t % 10 == 1 && t != 1) {
        sbTicks ++= "  "
      } else {
        if (t % 2 == 0) {
          sbTicks ++= (t % 10).toString
          sbTicks += ' '
        } else
          sbTicks ++= ". "
      }
    }

    //    res ++= "\n" + allocation.mkString(", ")

    List(sbJobs, sbIntervals, sbTicks).map(_.toString().take(2 * to).drop(2 * from)).mkString("\n")
  }

  def printSchedule(schedule: Scheduler#MulticoreSchedule, from: Int = 0, to: Int = 40): String = {
    val savedSchedule = schedule.take(2 * to).toList
    val cores = savedSchedule.head.keySet

    val sbJobs = new StringBuilder(2 * to)
    for (core <- cores) {
      val jobs = savedSchedule.map(_(core))
      val scheduledJobs = convertToScheduledJobs(jobs.toIterator)
      sbJobs.append(printSchedule(scheduledJobs, from, to))
      sbJobs.append("\n")
    }

    return sbJobs.toString()
  }
}*/

case class ScheduledJob(from: Int, to: Int, job: Job) {
  require(to > from, s"Incorrect interval. The end is smaller than the beginning: [$from, $to]. ")

  def length = to - from

  override def toString: String = job + "->" + from + ":" + to
}

case class ActiveJob(job: Job) {
  private var remainingTime = job.length

  def deadline = job.deadline

  def isCompleted = remainingTime == 0

  def isBusy = !isCompleted && remainingTime < job.length

  def execute(): ActiveJob = {
    if (isCompleted)
      throw new IllegalStateException(s"The job $job has been completed. Cannot execute it")

    val that = ActiveJob(job)
    that.remainingTime = remainingTime - 1
    that
  }
}

object IdleJob extends Job {
  override def release: Int = 0

  override def length: Int = Integer.MAX_VALUE

  override def deadline: Int = Integer.MAX_VALUE

  override def toString: String = "IJ"
}