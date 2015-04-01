package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model.{Job, Task}

/**
 * A job of a periodic task. Given index and the task, we compute other parameters
 * @param idx - index of a job. Must be greater or equal to 0
 * @param task - task that releases this job
 */
final case class PeriodicJob(idx: Int, task: Task) extends Job {

  val release = task.offset + idx*task.period
  val execution = task.execution
  val deadline = release + task.period

  override def toString: String = task.name + idx
}
