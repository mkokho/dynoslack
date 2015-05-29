package main.scala.org.kokho.rts.standard.impl.jobs

import main.scala.org.kokho.rts.standard.model.{Job, Task}

/**
 * A job of a periodic task. Given index and the task, we compute other parameters
 * @param idx - index of a job. Must be greater or equal to 0
 * @param ofTask - task that releases this job
 */
final case class PeriodicJob(idx: Int, private val ofTask: Task) extends Job {

  override def task: Option[Task] = Option(ofTask)

  val release = ofTask.offset + idx*ofTask.period
  val execution = ofTask.execution
  val deadline = release + ofTask.period

  override def toString: String = ofTask.name + idx
}
