package org.kokho.scheduling

import org.kokho.scheduling.PeriodicTask.PeriodicJob

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 *
 *          Task is an object that produces an infinite sequence of jobs.
 *
 *          A task is specified by four parameters:
 *          offset - the release time of the first job
 *          execution - the amount of time required to process a job
 *          deadline - a relative deadline of a job
 *          period - minimal period of time after which a next job can be released
 *
 *
 */
trait Task {

  type JobType <: Job

  def name: String = "NoName"

  def offset: Int

  def execution: Int

  def deadline: Int

  def period: Int

  final def utilization: Double = execution.toDouble / deadline

  /*
  //deadline is not overriden here by ImplicitDeadlineTask
  //therefore require method throws an exception
  require(offset >= 0, "Offset must be positive")
  require(execution > 0, "Execution must be non-negative")
  require(deadline > 0, "Deadline must be non-negative")
  require(period > 0, "Period must be non-negative")
  */

  def jobs(from: Int): Iterator[JobType]

  def jobs(): Iterator[JobType] = jobs(0)


  override def toString: String =
    if (offset == 0 && deadline == period)
      s"$name($execution in $period)"
    else
      s"$name($offset, $execution, $deadline, $period)"

}

/**
 * A task whose deadline equals to the period
 */
trait ImplicitDeadlineTask extends Task {
  val deadline = period
}

/**
 * A task that has offset 0
 */
trait SynchronousTask extends Task {
  val offset = 0
}

/**
 * A task that releases jobs at times k*period
 */
trait PeriodicTask extends Task {

  def convertJob(job: PeriodicJob): JobType

  override def jobs(from: Int): Iterator[JobType] = {
    val task = this

    val start = if (from <= task.offset) {
      0
    } else {
      //the job is produced at the end of the current period
      Math.ceil((from - task.offset).toDouble / period).toInt
    }

    Iterator.iterate(start)(_ + 1).map(
      idx => {
        val job = PeriodicJob(idx, task)
        convertJob(job)
      })
  }
}

object PeriodicTask {

  /**
   * This class enables equality of jobs produced by the same periodic task
   */
  sealed case class PeriodicJob(idx: Int, task: Task) extends Job {
    def release = idx * task.period

    def deadline = release + task.period

    def length = task.execution
  }

}
