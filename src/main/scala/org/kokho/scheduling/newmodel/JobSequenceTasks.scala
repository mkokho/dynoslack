package org.kokho.scheduling.newmodel

import org.kokho.scheduling.{Task, Job}

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
class JobSequenceTasks(val tasks: Seq[Task]) extends JobSequence {

  override def produce(): Iterator[Job] = tasks match {
    case Seq() => Iterator.empty
    case Seq(t) => t.jobs()
    case _ =>
      tasks
        .map(JobSequence(_))
        .reduce(_ merge _)
        .produce()
  }

}
