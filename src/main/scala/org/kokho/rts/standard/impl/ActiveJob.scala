package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model.Job

/**
 * Created with IntelliJ IDEA on 4/1/15.
 * @author: Mikhail Kokho
 */
final case class ActiveJob(job: Job, remaining: Int) extends Job {

  def this(job: Job) = this(job, job.execution)

  def release = job.release
  def execution = job.execution
  def deadline = job.deadline

  def execute(t: Int) = job match {
    case j:
    case _ => ActiveJob(job, remaining - t)
  }

  override def toString: String = "ActiveJob(" + job.toString + ", " + remaining + ")"
}
