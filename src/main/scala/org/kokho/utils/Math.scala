package org.kokho.utils

/**
 * Created by Mikhail Kokho on 6/1/2015.
 */
object Math {
  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

  def gcd(xs: Iterator[Int]): Int = foldInts(xs, gcd)

  def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)

  def lcm(xs: Iterator[Int]): Int = foldInts(xs, lcm)

  private def foldInts(ints: Iterator[Int], f: (Int, Int) => Int) = {
    require(ints.nonEmpty, "Collection must not be empty")

    ints.fold(ints.next())(f)
  }
}
