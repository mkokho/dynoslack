package org.kokho.scheduling_new.multicritical.schedulers

import org.kokho.scheduling_new.Job

/**
 * Created with IntelliJ IDEA on 6/26/15.
 * @author: Mikhail Kokho
 */
trait EdfOrdering {

  implicit def edfOrdering: Ordering[Job] = Ordering.by(_.deadline)

}
