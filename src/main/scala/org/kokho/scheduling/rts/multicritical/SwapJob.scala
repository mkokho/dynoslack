package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, JobProxy}

/**
 * Swap job is executed partially on a processor
 */
case class SwapJob(job: Job, swapTime: Int, executionPlan: List[Int], isFirstHalf: Boolean) extends JobProxy{

  override def toString: String = super.toString + " executed at " + executionPlan

  def isComplete = executionPlan.isEmpty

  def execute(t: Int) = {
    require(executionPlan.nonEmpty, "Swap job has already been completed")
    require(t == executionPlan.head, "The plan cannot be broken")
    new SwapJob(job, swapTime, executionPlan.tail, isFirstHalf)
  }
}
