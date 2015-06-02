package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.PeriodicTask.PeriodicJob
import org.kokho.scheduling._

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
sealed trait LoCriticalTask extends MulticriticalTask {

  override type JobType = LoCriticalJob

  def earlyReleases: List[Int]

  /**
   * History of early releases.
   *
   * Keeps the moments of time in decreasing order when a job was early released
   * @return
   */
  protected def historyOfEarlyReleases: List[Int]

  /**
   * Returns a new LoCritical tasks that releases jobs as this task before $time,
   * and after $time it releases the jobs at $time, $time + $task.period, $time + 2*$task.period
   *
   * @param time the moment of time when the early job is released
   * @return a new LoCriticalTasks that shifts releases of future jobs
   */
  def releaseEarlyJob(time: Int): LoCriticalTask = {
    require(canReleaseEarlyJob(time), s"Cannot release an early job at time $time")

    val newHistory = time :: historyOfEarlyReleases.dropWhile(_ > time)

    new LTaskWithRestrictedHistory(period, execution, earlyReleases, time)
  }

  def canReleaseEarlyJob(time: Int): Boolean = {
    //skip early releases that has not happened yet
    val before = historyOfEarlyReleases.dropWhile(_ > time)

    //calculate the early release time relative to the period
    val relativeRelease = if (before.isEmpty) time % period else (time - before.head) % period

    earlyReleases.contains(relativeRelease)
  }
}


object LoCriticalTask {

  def apply(period: Int, loExecution: Int): LoCriticalTask = apply(period, loExecution, loExecution.until(period).toList)

  def apply(period: Int, loExecution: Int, earlyReleases: List[Int]): LoCriticalTask = new LTaskWithoutEarlyReleases(period, loExecution, earlyReleases)

}


final class LTaskWithoutEarlyReleases (val period: Int, val execution: Int, val earlyReleases: List[Int]) extends LoCriticalTask with PeriodicTask {

  override protected val historyOfEarlyReleases: List[Int] = Nil

  override def convertJob(job: PeriodicJob): JobType = new LoCriticalJob(this, job)

}

final class LTaskWithRestrictedHistory (val period: Int, val execution: Int, val earlyReleases: List[Int], lastEarlyRelease: Int) extends LoCriticalTask {

  override protected val historyOfEarlyReleases: List[Int] =  List(lastEarlyRelease)

  override def jobs(from: Int): Iterator[JobType] = {
    require(from >= lastEarlyRelease, s"This task cannot produce jobs before time $lastEarlyRelease")
    
    val task = this

    val offset = lastEarlyRelease % period
    val start = offset + Math.ceil(period * (from - offset).toDouble / period).toInt


    Iterator.iterate(start)(_ + period).map(
      release => {
        LoCriticalJob(task, release, task.execution, release + task.period)
      })

/*
    new Iterator[JobType] {
      override def hasNext: Boolean = true

      var futureReleases = historyOfEarlyReleases.reverse.dropWhile(_ < from)
      var lastRelease = 0

      override def next(): JobType = {
        val nextRelease = futureReleases match {
          case nextEarlyRelease :: _ if nextEarlyRelease < lastRelease + task.period => nextEarlyRelease
          case _ => lastRelease + task.period
        }

        futureReleases = futureReleases.dropWhile(_ <= nextRelease)

        val newJob =  LoCriticalJob(task, nextRelease, task.execution, nextRelease + task.period)

      }
    }*/
  }

}


case class LoCriticalJob(private val task: LoCriticalTask, release: Int, length: Int, deadline: Int) extends Job {

  def this(task: LoCriticalTask, job: PeriodicJob) = this(task, job.release, job.length, job.deadline)

}
