package main.scala.org.kokho.rts.standard.impl

import main.scala.org.kokho.rts.standard.model._
import scala.collection.mutable
import scala.Some
import scala.collection.immutable.{Stack, Queue}

/**
 * @author Mikhail Kokho
 */
object EdfScheduler extends Scheduler{

  override def schedule[T <: Task](ts: TaskSystem[T]): Schedule = {
    require(ts.utilization <= 1, "Task system " + ts.mkString(",") + " has utilization greater than 1")


    def merge[J <: Job](xs: List[J], ys: List[J]):List[J] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xs1, y :: ys1) =>
        if (x.release < y.release) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val jobs = ts.jobs().buffered
    def releaseJobs(t:Int) = {
      var res = List[ActiveJob]()
      while(jobs.head.release <= t) res ::= new ActiveJob(jobs.next())
      //println("Releasing jobs at time " + t)
      //println("Results: " + res.mkString(" "))
      res
    }

    var queue = List[ActiveJob]()

    val itr:BufferedIterator[Job] = Iterator.from(0).map(t => {
      queue = merge(queue, releaseJobs(t)).dropWhile(_.remaining == 0).sortWith(_.deadline < _.deadline)
//      println("Scheduling jobs at time " + t)
//      println("Queue: " + queue.mkString(" "))
//      print(t + " ")
//      println(queue)
      if (queue.nonEmpty) {
        val j = queue.head
        queue = j.execute(1) :: queue.tail
        j.job
      } else IdleJob
    }).buffered

    new Schedule {
      private var from: Int = 0
      override def hasNext: Boolean = true

      override def next(): Allocation = {
        val j = itr.head
        var p = 0
        while (j == itr.head) {itr.next(); p += 1}
        from = from + p
        Allocation(from - p, from, j)
      }
    }
  }
}
