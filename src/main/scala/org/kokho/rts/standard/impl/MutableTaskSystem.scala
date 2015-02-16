package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model.{Task, TaskSystem}
import scala.collection.SetLike
import scala.collection.mutable
import scala.collection

/**
 * Created with IntelliJ IDEA on 11/13/14.
 * @author: Mikhail Kokho
 */
class MutableTaskSystem[T <: Task] private
  extends TaskSystem[T] with SetLike[T, MutableTaskSystem[T]] {

  private var tasks: Set[T] = Set[T]()

  def contains(elem: T): Boolean = tasks.contains(elem)

  def +(elem: T): MutableTaskSystem[T] = {
    tasks += elem
    this
  }

  def -(elem: T): MutableTaskSystem[T] = {
    tasks -= elem
    this
  }

  def iterator: Iterator[T] = tasks.iterator

  override def empty: MutableTaskSystem[T] = MutableTaskSystem()
}



object MutableTaskSystem {

  def apply[T <: Task]() = new MutableTaskSystem[T]()

}