package org.kokho.scheduling

/**
 * Schedule is an infinite sequence of scheduled jobs.
 *
 * The scheduled jobs, returned by the method iterate(), must be consecutive
 *
 * @author: Mikhail Kokho
 * @date 7/2/15.
 */
trait Schedule extends Iterable[Seq[ScheduledJob]]{

}
