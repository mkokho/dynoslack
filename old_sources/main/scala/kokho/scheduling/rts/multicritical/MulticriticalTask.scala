package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{PeriodicTask, SynchronousTask, ImplicitDeadlineTask, Task}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait MulticriticalTask extends Task
with ImplicitDeadlineTask
 {

}
