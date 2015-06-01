package org.kokho.binpacking

import org.kokho.scheduling.exceptions.UnpackableSetException

import scala.collection.mutable

/**
 * Created by Mikhail Kokho on 5/29/2015.
 */
class FirstFitPacker extends BinPacker{

  /**
   * Applies First-Fit heuristic to solve Bin Packing problem
   *
   * @param objects objects to be distributed in the bins
   * @return a sequence of sets where each set contains some of the objects
   * @throws UnpackableSetException if the algorithm fails to find feasible packing
   */
  override def packObjects[T](objects: Set[T])(implicit wrap: T => WeightedObject): Seq[Set[T]] = {
    throwIfOverweighted(objects)

    //we need at most object.size number of bins
    val bins = Seq.fill(objects.size)(mutable.ListBuffer[T]())

    for {obj <- objects} {
      val freeBin = bins.dropWhile(freeSpace(_) < obj.weight).head
      freeBin += obj
    }

    val placedNum = bins.flatten.size
    if (objects.size != placedNum) {
      throw new UnpackableSetException(s"First-fit partitioning failed: not all objects has been packed")
    }

    bins.filter(_.nonEmpty).map(_.toSet)
  }

}
