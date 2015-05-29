package org.kokho.scheduling

import org.kokho.scheduling.exceptions.UnpackableSetException

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA on 5/29/15.
 * @author: Mikhail Kokho
 */
trait Partitioner{

  private def volume[T <: WeightedObject](bin: Set[T]): Double = bin.map(_.weight).sum

  /**
   * @param objects objects to be partitioned
   * @param binsNum number of bins available
   * @return a list of sets such that the sum of objects' weight in each set does not exceed 1,
   *         or throws UnpackableSetException
   */
  def firstFitPartition[T <: WeightedObject](objects: Set[T], binsNum: Int): Seq[Set[T]] = {
    objects foreach { obj: T =>
      if (obj.weight > 1)
        throw new UnpackableSetException(s"A weight of $obj exceeds 1. Cannot put it in any bin of volume 1")
    }

    val sum = objects.map(_.weight).sum
    if (sum > binsNum)
      throw new UnpackableSetException(s"Weight of object exceeds the number of bins: $sum > $binsNum")

    val partition = Seq.fill(binsNum)(mutable.ListBuffer[T]())

    for {
      obj <- objects
      bin <- partition if bin.map(_.weight).sum < 1 - obj.weight
    } {
      bin += obj
    }

    val placedNum = partition.flatten.size
    if (objects.size != placedNum) {
      throw new UnpackableSetException(s"First-fit partitioning failed: not all objects has been packed")
    }

    partition.map(_.toSet)
  }
}

trait WeightedObject{

  def weight: Double

}
