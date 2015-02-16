package main.scala.org.kokho.rts.standard.model

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */

trait Job{

  def release: Int
  def execution: Int
  def deadline: Int

  override def toString: String = super.toString
}

object IdleJob extends Job {
  val deadline: Int = Int.MaxValue
  val execution: Int = 0
  val release: Int = 0

  override def toString: String = "IdleJob"
}


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

final case class ActiveJob(job: Job, remaining: Int) extends Job {

  def this(job: Job) = this(job, job.execution)

  def release = job.release
  def execution = job.execution
  def deadline = job.deadline

  def execute(t: Int) = ActiveJob(job, remaining - t)

  override def toString: String = "ActiveJob(" + job.toString + ", " + remaining + ")"
}

/*
case class Job(idx: Int, task: Task) {

  val start = idx * task.period
  val length = task.exec
  val deadline = start + task.period

  override def toString: String = task.name + idx
}

object DeadlineOrder extends Ordering[Job]{

  def compare(a: Job, b: Job) = a.deadline - b.deadline

}
*/
