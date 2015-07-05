package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 6/4/15.
 * @author: Mikhail Kokho
 */

/**
 * When there are no active jobs to execute, processor is idle.
 * IdleJob represents idle time on a processor
 */
object IdleJob extends Job {
  override def release: Int = 0

  override def length: Int = Integer.MAX_VALUE

  override def deadline: Int = Integer.MAX_VALUE

  override def toString: String = "IJ"

}
