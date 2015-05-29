package main.scala.org.kokho.rts.standard.model

import main.scala.org.kokho.rts.utils.Math.lcm
import scala.collection.Set
import scala.util.Sorting


/**
 * @author Mikhail Kokho
 */
trait TaskSystem[T <: Task] extends Set[T] {

//  protected def tasks: Set[T] = this.toSet

  /**
   * @return an iterator of jobs in order of their release times
   */
  def jobs(): Iterator[T#JobType] = {

    implicit val localJobOrdering:Ordering[T#JobType]
      = Ordering[(Int, String)].on(j => (j.release, j.toString))

    def msort[J](xs: BufferedIterator[J], ys: BufferedIterator[J])(implicit ord: Ordering[J]): BufferedIterator[J] = {
      if (xs.isEmpty) ys
      else if (ys.isEmpty) xs
      else {
        var res:Iterator[J] = Iterator.empty

        if (ord.lt(xs.head, ys.head))
          res = Iterator(xs.next()) ++ msort(xs, ys)(ord)
        else
          res = Iterator(ys.next()) ++ msort(xs, ys)(ord)

        res.buffered
      }
    }

    this.map(_.jobs().buffered).fold(Iterator.empty.buffered)(msort[T#JobType])
  }

  def hyperPeriod: Int = lcm(this.iterator.map(_.period))

  def utilization:Double = this.map(_.utilization).sum
}






