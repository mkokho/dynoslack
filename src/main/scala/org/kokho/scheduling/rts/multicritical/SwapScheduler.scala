package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{UniformCore, Core, Task, Scheduler}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapScheduler(hiTasks:Set[HiCriticalTask], loTasks: Set[LowCriticalTask], coreNumber: Int) extends Scheduler{

  override val cores: Seq[Core] = 0.until(coreNumber).map(UniformCore)

  override def tasks: Set[Task] = hiTasks ++ loTasks

  override def schedule(): MulticoreSchedule = ???

  override def isSchedulable: Boolean = ???

}