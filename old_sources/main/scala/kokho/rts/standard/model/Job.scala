package main.scala.org.kokho.rts.standard.model

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */

trait Job{

  def release: Int
  def execution: Int
  def deadline: Int

  def task:Option[Task] = None

  override def toString: String = "Job(" + execution + "->" + release + ":" + deadline + ")"
}

object IdleJob extends Job {
  val deadline: Int = Int.MaxValue
  val execution: Int = 0
  val release: Int = 0

  override def toString: String = "IJ"
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
