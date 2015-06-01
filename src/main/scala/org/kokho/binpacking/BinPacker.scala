package org.kokho.binpacking

import org.kokho.scheduling.exceptions.UnpackableSetException

/**
 * Created with IntelliJ IDEA on 5/29/15.
 * @author: Mikhail Kokho
 */
trait BinPacker {

  /**
   * Approximation algorithm that solves Bin Packing problem
   *
   * @param objects objects to be distributed in the bins
   * @return a sequence of sets where each set contains part of the objects
   * @throws UnpackableSetException if the algorithm fails to find feasible packing
   */
  def packObjects[T](objects: Set[T])(implicit wrap: T => WeightedObject): Seq[Set[T]]

  /**
   * Calculates the residual capacity of the bin
   *
   * @param bin a set of objects
   * @return 1 minus the sum of objects' weight
   */
  protected def freeSpace[T](bin: Traversable[T])(implicit wrap: T => WeightedObject): Double = {
    1 - bin.map(_.weight).sum
  }


  /**
   * Throws UnpackableSetException if the set of objects contains an element with
   * weight greater than 1
   */
  protected def throwIfOverweighted[T](objects: Set[T])(implicit wrap: T => WeightedObject) = {
    objects foreach { obj: T =>
      if (obj.weight > 1)
        throw new UnpackableSetException(s"A weight of $obj exceeds 1. " +
          "Cannot put it in any bin of volume 1")
    }
  }
}

object BinPacker {
  def apply() = new FirstFitPacker()
}


