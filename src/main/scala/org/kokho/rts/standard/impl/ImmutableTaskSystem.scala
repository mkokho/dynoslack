package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model.{TaskSystem, Task}
import scala.collection.{SetLike, immutable}

/**
 * Created with IntelliJ IDEA on 11/13/14.
 * @author: Mikhail Kokho
 */
class ImmutableTaskSystem[T <: Task] private (val tasks: immutable.Set[T])
  extends TaskSystem[T] with SetLike[T, ImmutableTaskSystem[T]] {

  def contains(elem: T): Boolean = tasks.contains(elem)

  def +(elem: T): ImmutableTaskSystem[T] =
    if (contains(elem)) this
    else new ImmutableTaskSystem[T](tasks + elem)

  def -(elem: T): ImmutableTaskSystem[T] =
    if (!contains(elem)) this
    else new ImmutableTaskSystem[T](tasks - elem)


  override def empty: ImmutableTaskSystem[T] = ImmutableTaskSystem[T]()

  def iterator: Iterator[T] = tasks.iterator
}

object ImmutableTaskSystem {

  def apply[T <: Task]() = new ImmutableTaskSystem[T](Set())

}