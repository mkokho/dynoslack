package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicJob, PeriodicTask}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * Children of the same parent with the same offset are equal
 */
private final case class LoCriticalTaskChild (parent: LoCriticalTaskParent, offset: Int)
  extends LoCriticalTask
  with PeriodicTask {

  override def execution: Int = parent.execution

  override def period: Int = parent.period

  override def earlyReleases: List[Int] = parent.earlyReleases

  override def convertJob(job: PeriodicJob): JobType = new LoCriticalJob(this, job)

  override def name: String = parent.name

  override def isChildOf(thatTask: LoCriticalTask): Boolean = thatTask match {
    case t: LoCriticalTaskParent => this.parent == t
    case t: LoCriticalTaskChild => this.parent == t.parent
  }

  override def shiftedTasks(time: Int): LoCriticalTask = {
    require(canReleaseEarlyJob(time), s"Cannot release job at time $time")

    new LoCriticalTaskChild(this.parent, time)
  }
}
