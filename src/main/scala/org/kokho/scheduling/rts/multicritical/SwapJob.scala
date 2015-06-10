package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, JobProxy}

/**
 * Swap job is executed partially on a processor
 */
case class SwapJob(job: Job, executionPlan: List[Int]) extends JobProxy{
//  require(reclaimingPlan.nonEmpty, "Slack reclamation plan must be known in advance")

//  override def length: Int = reclaimingPlan.head
  override def toString: String = super.toString + " executed at " + executionPlan

  def isComplete = executionPlan.isEmpty

  def execute(t: Int) = {
    require(executionPlan.nonEmpty, "Swap job has already been completed")
    require(t == executionPlan.head, "The plan cannot be broken")
    new SwapJob(job, executionPlan.tail)
  }
}
