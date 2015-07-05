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




