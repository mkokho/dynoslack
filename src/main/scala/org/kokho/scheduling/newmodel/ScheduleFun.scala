package org.kokho.scheduling.newmodel

import org.kokho.scheduling.{IdleJob, ScheduledJob, ActiveJob, Job}

import scala.collection.immutable.ListMap
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */

abstract class ScheduleFun(val jobs: JobSequence) {

  private var globalTime = 0

  private val activeJobs: mutable.PriorityQueue[ActiveJob] = mutable.PriorityQueue.empty

  private val executionState: mutable.Map[Job, Int] = mutable.HashMap.empty

  /**
   * Determines the order in which jobs are being executed
   */
  implicit def priority: Ordering[ActiveJob]


  private def releaseJobs() = activeJobs ++ jobs.produceAt(globalTime)

  private def executeJob(): Job =
    if (activeJobs.isEmpty) IdleJob
    else {
      //      val job = activeJobs.head
      //      val time = executionState.getOrElse(job, 0)
      ???
    }


  def execute(): ScheduledJob = {
    val nextJob =
      if (activeJobs.isEmpty)
        IdleJob
    //      else

    ???

  }
}
