package org.kokho.scheduling.newmodel

import org.kokho.scheduling.rts.multicritical.LoCriticalTask
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/26/15.
 * @author: Mikhail Kokho
 */
class JobSequenceWithERTestSuite extends FlatSpec with JobSequenceBehavior {

  val loTask = LoCriticalTask(10, 4, List(6))
  val jobSeq = new JobSequenceWithER(loTask)


  "A Job Sequence with ER" must behave like aJobSequence(jobSeq)

  it must "change output after release of an early job" in {
    val release: Int = loTask.earlyReleases.head
    jobSeq.releaseEarly(release)
    val res = jobSeq.produce().take(1).toList
    assert(res.nonEmpty && res.head.release == release, s"Early job has not been released: $res")

  }

}
