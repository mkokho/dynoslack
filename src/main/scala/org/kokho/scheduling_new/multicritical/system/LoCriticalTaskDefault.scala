package org.kokho.scheduling_new.multicritical.system

import org.kokho.scheduling.{PeriodicJob, PeriodicTask}

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTaskDefault(val period: Int,
                           val execution: Int,
                           val earlyReleases: List[Int],
                           val offset: Int = 0,
                           val relatedTo: Option[LoCriticalTask] = None)
  extends LoCriticalTask {

  require(earlyReleases.forall(release => release >= execution && release < period))

  override def shift(time: Int): LoCriticalTask = {
    require(canReleaseEarlyJob(time) || toRelativeTime(time) == 0, s"Cannot release job at time $time")

    val relationWith = relatedTo.getOrElse(this)

    new LoCriticalTaskDefault(period, execution, earlyReleases, time, Some(relationWith))
  }

  //  override def isChildOf(thatTask: LoCriticalTask): Boolean = false
}
