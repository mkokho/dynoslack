package main.scala.org.kokho.rts.model

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
trait Scheduler {

  type Schedule = Iterator[Allocation]

  def schedule[T <: Task](ts: TaskSystem[T]):Schedule

}

case class Allocation(from: Int, to: Int, job: Job) {
  require(to > from, "Allocation must have from greater than to")

  def length = to - from
}
