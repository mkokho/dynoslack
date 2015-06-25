package org.kokho.scheduling.newmodel

import org.kokho.scheduling.{Task, Job}

/**
 * Created with IntelliJ IDEA on 6/25/15.
 * @author: Mikhail Kokho
 */

/**
 * Trait representing a sequence of jobs.
 */
trait JobSequence {
  self =>

  /**
   * Returns jobs in order of their release times
   */
  def produce(): Iterator[Job]


  /**
   * Returns jobs that are released after or at $from in order of their release times
   */
  def produce(from: Int): Iterator[Job] = produce().dropWhile(_.release < from)

  /**
   * Returns jobs that are released in the period $from to $to (both inclusive)
   */
  def produce(from: Int, to: Int) = produce(from).takeWhile(_.release <= to)

  def produceAt(time: Int) = produce(time, time)

  /**
   * Merges two job sequences into one preserving the order of jobs
   */
  def merge(that: JobSequence): JobSequence = new JobSequence {
    override def produce(): Iterator[Job] = {
      val jobsThat = that.produce().buffered
      val jobsSelf = self.produce().buffered
      mergeJobs(jobsSelf, jobsThat)
    }
  }


  private def insertJob(j: Job, itr: BufferedIterator[Job]): Iterator[Job] =
    mergeJobs(Iterator(j).buffered, itr)


  private def mergeJobs(xs: BufferedIterator[Job], ys: BufferedIterator[Job]): Iterator[Job] = new Iterator[Job] {
    override def hasNext: Boolean = xs.nonEmpty && ys.nonEmpty

    override def next(): Job = {
      if (xs.isEmpty) ys.next()
      else if (ys.isEmpty) xs.next()
      else {
        if (xs.head.release <= ys.head.release)
          xs.next()
        else
          ys.next()
      }
    }
  }





}

object JobSequence {

  val empty = new JobSequence {
    override def produce(): Iterator[Job] = Iterator.empty
  }

  def apply(t: Task): JobSequence = apply(Seq(t))

  def apply(tasks: Seq[Task]): JobSequence = new JobSequenceTasks(tasks)

}
