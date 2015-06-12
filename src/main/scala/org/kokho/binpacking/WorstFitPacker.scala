package org.kokho.binpacking

import org.kokho.scheduling.exceptions.UnpackableSetException

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
class WorstFitPacker extends BinPacker{
  /**
   * Applies Worst-Fit heuristic to approximate Bin Packing problem
   *
   * @param objects objects to be distributed in the bins
   * @return a sequence of sets where each set contains part of the objects
   * @throws UnpackableSetException if the algorithm fails to find feasible packing
   */
  override def packObjects[T](objects: Set[T])(implicit wrap: (T) => WeightedObject): Seq[Set[T]] =
    packObjectsGen(objects)(wrap)



  private def packObjectsGen[T](objects: Traversable[T])(implicit wrap: (T) => WeightedObject): Seq[Set[T]] = {
    throwIfOverweighted(objects)

    def listToUtilization (l: List[T]) = l.map(_.weight).sum

    def packHelper(unpackedObjects: List[T], bins: List[List[T]]): List[List[T]] = unpackedObjects match {
      case Nil => bins
      case obj :: otherObjs => {
        val sorted = bins.sortBy(listToUtilization)
        val worstBin = sorted.head
        if (listToUtilization(worstBin) + obj.weight <= 1)
          packHelper(otherObjs, (obj :: worstBin) :: sorted.tail )
        else
          packHelper(otherObjs, List(obj) :: sorted)
      }
    }

    packHelper(objects.toList, List(List())).map(_.toSet)
  }
}
