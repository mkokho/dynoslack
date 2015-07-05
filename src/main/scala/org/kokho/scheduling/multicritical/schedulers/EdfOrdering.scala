package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.Job

/**
 * Created with IntelliJ IDEA on 6/26/15.
 * @author: Mikhail Kokho
 */
trait EdfOrdering {

  implicit def edfOrdering: Ordering[Job] = Ordering.by(_.deadline)

}
