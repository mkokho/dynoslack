package org.kokho.binpacking

import org.scalatest.FlatSpec

/**
 * Created by Mikhail Kokho on 6/12/2015.
 */
class WorstFitPackerTestSuite extends FlatSpec with BinPackerBehavior {

  val wfPacker = new WorstFitPacker {}

  "A first fit packing algorithm" should behave like aBinPacker(wfPacker)

  it should "use two bins for the sequence 0.8, 0.6, 0.4, 0.2" in {
    val objects = Set(0.8, .6,.4,.2)
    val bins = wfPacker.packObjects(objects)
    assert(bins.size == 2)
  }
}
