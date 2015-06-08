package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.ScheduleBehavior
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/5/15.
 * @author: Mikhail Kokho
 */
trait MulticriticalScheduleBehavior extends ScheduleBehavior{
  this: FlatSpec =>

  type Partition = Seq[Seq[MulticriticalTask]]

  val isOdd: Int => Boolean = (x: Int) => x % 2 == 1


  implicit def tupleToPartition(tuple: Product): Partition = {
    tuple.productIterator.toSeq.map(_.asInstanceOf[Seq[MulticriticalTask]])
  }

  implicit def tupleToSchedule(tuple: Product): MulticriticalSchedule = {
    val partition = tuple.productIterator.map(_.asInstanceOf[Seq[MulticriticalTask]])
    toSchedule(partition.toSeq)
  }

  implicit def seqToSchedule(seq: Seq[MulticriticalTask]): MulticriticalSchedule =
    toSchedule(Seq(seq))

  implicit def partitionToSchedule(partition: Partition): MulticriticalSchedule =
    toSchedule(partition)

  def toSchedule(tasks: Partition): MulticriticalSchedule


  def aMulticriticalSchedule(sch: MulticriticalSchedule): Unit ={
    
    it should behave like aSchedule(sch)
    
  }

}
