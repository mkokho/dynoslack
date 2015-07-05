package org.kokho.scheduling

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */
trait JobStreamBehavior extends Matchers {
  this: FlatSpec =>

  def aNonEmptyJobStream(js: JobStream): Unit = {
    val timeLimit = 50000

    it must "produce non empty iterator" in {
      js.produce().nonEmpty shouldBe true
    }

    it must "produce jobs in order of their release time" in {
      val pairs = js.produce().take(timeLimit).sliding(2)
      val wrongPair = pairs.find { case Seq(x, y) => x.release > y.release}

      assert(wrongPair.isEmpty, s"Jobs must be produced in order of their release time. Incorect: $wrongPair")
    }

    it must "produce jobs after some moment of time" in {
      val from = Random.nextInt(timeLimit / 2)
      val jobs = js.produce(from).take(timeLimit)
      val wrongJobs = jobs.filter(_.release < from)

      assert(wrongJobs.isEmpty, s"Job Sequence must produce jobs from the specified moment of time")
    }

    it must "produce jobs  in the specified period" in {
      val from = Random.nextInt(100)
      val to = from + Random.nextInt(10)
      val wrongJobs = js.produce(from, to)
        .filter(j => j.release < from || j.release > to).toList

      assert(wrongJobs.isEmpty, s"Test failed: jobs $wrongJobs are out of [$from, $to]")
    }

  }
  
  def anEmptyJobStream(js: JobStream): Unit = {

    it must "produce an empty iterator" in {
      js.produce().isEmpty shouldBe true
    }

  }

}