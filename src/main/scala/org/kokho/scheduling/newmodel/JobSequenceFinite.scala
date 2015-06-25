package org.kokho.scheduling.newmodel

import org.kokho.scheduling.Job

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */

/**
 * Job Sequence wrapped over a collection of jobs
 */
class JobSequenceFinite(val col: List[Job]) extends JobSequence{

  override def produce(): Iterator[Job] = col.sortBy(_.release).iterator

}
