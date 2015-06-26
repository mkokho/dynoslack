package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicJob, PeriodicTask}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTaskMutable(val period: Int,
                            val execution: Int,
                            val earlyReleases: List[Int])
  extends LoCriticalTask
  with PeriodicTask {

  require(earlyReleases.forall(release => release >= execution && release < period))

  private var currentOffset = 0

  /**
   * The first low critical task has offset 0
   */
  override def offset = currentOffset

  override def convertJob(job: PeriodicJob): JobType = new LoCriticalJob(this, job)

  override def shiftedTasks(time: Int): LoCriticalTask = {
    require(canReleaseEarlyJob(time), s"Cannot release job at time $time")

    currentOffset = time
    this
  }

  override def isChildOf(thatTask: LoCriticalTask): Boolean = false
}
