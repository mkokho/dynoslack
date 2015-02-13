package test.scala.org.kokho.rts

import org.scalatest.FlatSpec
import main.scala.org.kokho.rts.model.Task
import main.scala.org.kokho.rts.impl.BasicTask

/**
 * @author Mikhail Kokho
 */
abstract class TaskSpec extends FlatSpec{
  def task:Task

  behavior of "A task"

  it must "produce non empty sequence of jobs" in {
    assert(task.jobs().nonEmpty)
  }

  it must "release jobs no earlier than the period" in {
    val jobs = task.jobs()
    val j1 = jobs.next()
    val j2 = jobs.next()

    assert(j2.release >= j1.deadline, "Second Job must be released at the deadline of the first")
  }

  it must "release the first job at the moment offset" in {
    assert(task.jobs().next().release == task.offset,
      "The release time of the first job does not equal offset of the task")
  }

  it must "produce jobs in order of their release times" in {
    val jobs = task.jobs().buffered
    var jobPrev = jobs.next()
    for (i <- Range(0, 1000)){
      assertResult(jobPrev.deadline){ jobs.head.release }
      jobPrev = jobs.next()
    }
  }

}

class BasicTaskSpec extends TaskSpec {
  val task = BasicTask("A", 4, 10)
}
