package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, JobProxy}

/**
 * Swap job is executed partially on a processor
 */
case class SwapJob(job: Job) extends JobProxy{
//  require(reclaimingPlan.nonEmpty, "Slack reclamation plan must be known in advance")

//  override def length: Int = reclaimingPlan.head
}
