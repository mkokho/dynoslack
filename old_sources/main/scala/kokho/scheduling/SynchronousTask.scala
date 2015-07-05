package org.kokho.scheduling

/**
 * A task that has offset 0
 */
trait SynchronousTask extends Task {
  val offset = 0
}
