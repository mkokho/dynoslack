package org.kokho.binpacking

import org.scalatest.FlatSpec

/**
 * Created by Mikhail Kokho on 5/29/2015.
 */
class FirtFitPackerTestSuite extends FlatSpec with BinPackerBehavior{

  val ffPacker = new FirstFitPacker {}

  "A first fit packing algorithm" should behave like aBinPacker(ffPacker)

}
