package org.kokho.scheduling_new.multicritical.system

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
    require(canReleaseEarlyJob(time), s"Task ${this} cannot release an early job at time $time")

    val relationWith = relatedTo.getOrElse(this)

    new LoCriticalTaskDefault(period, execution, earlyReleases, time, Some(relationWith))
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: LoCriticalTaskDefault => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  def canEqual(a: Any) = a.isInstanceOf[LoCriticalTaskDefault]

  override def hashCode:Int = {
    //tasks that are not children of any other tasks are different
    if (relatedTo.isEmpty) super.hashCode
    //otherwise they are equal if has the same parameters
    else {
      val prime = 31
      var result = 1
      result = prime * result + period
      result = prime * result + execution
      result = prime * result + offset
      result = prime * result + earlyReleases.hashCode()
      result = prime * result + relatedTo.get.hashCode()
      result
    }
  }
}
