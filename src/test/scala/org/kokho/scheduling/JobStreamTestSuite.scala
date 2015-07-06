package org.kokho.scheduling

import org.kokho.scheduling.JobStream._
import org.kokho.scheduling.multicritical.system.HiCriticalTask
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
class JobStreamTestSuite extends FlatSpec with JobStreamBehavior{

  "An Empty JobStream" must behave like anEmptyJobStream(JobStream.empty)

  val taskA = HiCriticalTask(9, 5, 5)
  val taskB = HiCriticalTask(7, 5, 5)
  val taskC = HiCriticalTask(3, 1, 1)

  "A Job Sequence of one task" must behave like aNonEmptyJobStream(taskA)

  "A merged job sequence" must behave like aNonEmptyJobStream(taskA.merge(taskB))

  "A merged job sequence of three tasks" must behave like aNonEmptyJobStream(Seq(taskA, taskB, taskC))

  "A job stream (when merged with an empty stream)" must "produce the same jobs" in {
    val empty = JobStream.empty
    val jobs = taskA.jobs().take(2).toList
    val stream = toJobStream(jobs)

    stream.merge(empty).produce().toList shouldEqual jobs
    empty.merge(stream).produce().toList shouldEqual jobs

  }

}
