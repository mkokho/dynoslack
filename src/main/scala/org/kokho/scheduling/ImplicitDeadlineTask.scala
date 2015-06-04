package org.kokho.scheduling

/**
 * A task whose deadline equals to the period
 */
trait ImplicitDeadlineTask extends Task {
  val deadline = period
}
