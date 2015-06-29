package org.kokho.scheduling_new

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 *
 *          Task is an immutable object that produces an infinite sequence of jobs.
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

  /**
   * the type of jobs released by this task
   */
  type JobType <: Job

  def offset: Int

  def execution: Int

  def deadline: Int

  def period: Int

  final def utilization: Double = execution.toDouble / deadline


  def name: String = "T"


  /**
   * Generates the jobs of this task starting at time $from
   *
   * @param from a non negative number that bounds the release time of the first job.
   *             All produces jobs has release time greater than $from
   * @return an iterator of jobs
   * @throws IllegalArgumentException if $from is smaller than 0
   */
  def jobs(from: Int): Iterator[JobType]

  /**
   * Generates all jobs released by this task
   */
  def jobs(): Iterator[JobType] = jobs(this.offset)

  /**
   * Drops $n jobs and returns the next job
   * 
   * @param n the number of jobs to drop. Must be non negative
   */
  def job(n: Int): JobType = {
    require(n >= 0)
    jobs().drop(n).next()
  }

  override def toString: String =
    if (offset == 0 && deadline == period)
      s"$name($execution in $period)"
    else
      s"$name($offset, $execution, $deadline, $period)"

}








