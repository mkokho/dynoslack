package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.PartitionerBehavior
import org.scalatest.{FlatSpec}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapSchedulerTestSuite extends FlatSpec with PartitionerBehavior{

  val loSet = Set[LoCriticalTask]() +
    new LoCriticalTask(10, 4, List(6,8))


  val hiSet = Set[HiCriticalTask]() +
    new HiCriticalTask(10, 4, 6)

  val swapScheduler = new SwapScheduler(hiSet, loSet, 2)

  "A SwapScheduler" should behave like aPartitioner(swapScheduler)

}
