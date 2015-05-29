package org.kokho.scheduling

import org.kokho.scheduling.exceptions.UnpackableSetException
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 5/29/15.
 * @author: Mikhail Kokho
 */
trait PartitionerBehavior { this: FlatSpec =>

  implicit def doubleToWeighted(x: Double):WeightedObject = new WeightedObject {
    override def weight: Double = x
  }

  private def toWeightedSet(nums: Double*) = nums.map(doubleToWeighted).toSet

  val bigObject = 1.2
  val justFit = 1.0

  def aPartitioner(partitioner: Partitioner): Unit = {

    it must "throw an exception when an object weight exceeds 1" in {
      intercept[UnpackableSetException] {
        partitioner.firstFitPartition(toWeightedSet(bigObject), 1)
      }
    }

  }

}
