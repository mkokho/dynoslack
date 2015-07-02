package org.kokho.scheduling_new

/**
 * Schedule is an infinite sequence of scheduled jobs.
 *
 * The scheduled jobs, returned by the method next(), must be consecutive
 *
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
trait Schedule extends Iterable[Seq[ScheduledJob]]{
//  override def hasNext: Boolean = true
}
