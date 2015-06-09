package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{Job, JobProxy}

/**
 * Swap job is executed partially on a processor
 * @param reclaimedSlack - how long the job is being executed on a processor
 */
case class SwapJob(job: Job, reclaimedSlack: Int) extends JobProxy{

  override def length: Int = reclaimedSlack
}
