package org.kokho.scheduling.mockobjects

import org.kokho.scheduling.{Job, Core, UniformCore, Scheduler}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait OneCoreScheduler extends Scheduler{

  override def cores: Seq[Core] = List(core)

  val core = UniformCore(0)

}
