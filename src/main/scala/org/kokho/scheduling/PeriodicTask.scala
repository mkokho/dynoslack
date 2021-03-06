package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * A task that releases jobs periodically.
 *
 * The first job is released at the moment $task.offset.
 * Each subsequent job is released at the moment equal to the deadline of the previous job.
 * In other words, deadline of the task is equal to its period
 */
trait PeriodicTask extends Task {
  self =>

  override def deadline: Int = period

  override def jobs(from: Int): Iterator[JobType] = {
    val idx = if (from <= self.offset) {
      0
    } else {
      //the job is produced at the end of the current period
      Math.ceil((from - self.offset).toDouble / period).toInt
    }

    val start = self.offset + idx*period

    Iterator.from(start, self.period).map(buildJob)
  }

  def buildJob(release: Int): JobType
}
