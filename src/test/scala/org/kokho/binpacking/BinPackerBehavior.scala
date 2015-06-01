package org.kokho.binpacking

import org.kokho.scheduling.exceptions.UnpackableSetException
import org.scalatest.FlatSpec

import scala.util.Random

/**
 * Created with IntelliJ IDEA on 5/29/15.
 * @author: Mikhail Kokho
 */
trait BinPackerBehavior { this: FlatSpec =>

  implicit def doubleToWeighted(x: Double):WeightedObject = new WeightedObject {
    override def weight: Double = x
  }

  def aBinPacker(packer: BinPacker): Unit = {

    it must "throw an exception when an object weight exceeds 1" in {
      val bigObject = 1.2
      intercept[UnpackableSetException] {
        packer.packObjects(Set(bigObject))
      }
    }

    it must "use one bin for one object" in {
      val bins = packer.packObjects(Set(0.5))
      assert(bins.size == 1)
    }

    it must "use k bins for k objects of weight > 0.5" in {
      val objects = Iterator.iterate(0.51)(_ + 0.01).take(5).toSet
      val bins = packer.packObjects(objects)
      assert(bins.size == objects.size)
    }

    it must "put two small objects into the same bin" in {
      val objects = Set(0.1, 0.2)
      val bins = packer.packObjects(objects)
      assert(bins(0) == objects)
    }

    it must "distribute all objects" in {
      val objects = List.fill(10)(Random.nextDouble() / 2).toSet
      val bins = packer.packObjects(objects)
      val diff = objects.diff(bins.flatten.toSet)
      assert(diff.isEmpty, s"These objects has not been packed: $diff. All objects: $objects")
    }
  }
}
