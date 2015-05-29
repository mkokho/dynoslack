package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 5/29/15.
 * @author: Mikhail Kokho
 */
class EdfScheduler(val tasks: Set[Task], coreNum: Int) extends Scheduler {

  override def schedule(): MulticoreSchedule = {



    ???
  }

  override def isSchedulable: Boolean = isFeasible && {
    if (cores.size == 1) {
      true
    } else {
      //here we need to do partition analysis
      true
    }
  }

  override def cores: Seq[Core] = 0.until(coreNum).map(UniformCore)
}
