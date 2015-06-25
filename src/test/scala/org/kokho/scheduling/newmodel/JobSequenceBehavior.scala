package org.kokho.scheduling.newmodel

import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
//class JobSequenceBehavior extends Matchers{
trait JobSequenceBehavior extends  Matchers  {
  this: FlatSpec =>

  def aJobSequence(js: JobSequence): Unit = {
    val timeLimit = 50000

    it must "produce jobs in order of their release time" in {
      val pairs = js.produce().take(timeLimit).sliding(2)
      val wrongPair = pairs.find { case Seq(x, y) => x.release > y.release}

      assert(wrongPair.isEmpty, s"Jobs must be produced in order of their release time. Incorect: $wrongPair")
    }

  }

}
