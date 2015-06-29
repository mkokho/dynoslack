package org.kokho.scheduling_new

/**
 * Trait representing a stream of jobs.
 *
 * It is a utility class for the schedulers.
 */
trait JobStream {
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
  def produce(from: Int, to: Int): Iterator[Job] = produce(from).takeWhile(_.release <= to)

  /**
   * Returns jobs that are releases exactly at  $time
   */
  def produceAt(time: Int): Iterator[Job] = produce(time, time)

  /**
   * Merges two job sequences, preserving the order of produced jobs
   */
  def merge(that: JobStream): JobStream = new JobStream {
    override def produce(): Iterator[Job] = {
      val jobsThat = that.produce().buffered
      val jobsSelf = self.produce().buffered
      mergeJobs(jobsSelf, jobsThat)
    }
  }

  /**
   * Inserts a job into this job sequence
   */
  def insert(job: Job): JobStream = merge(job)


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

/**
 * The companion object for the trait JobStream.
 *
 * It contains implicit conversions of some collections of jobs to JobStream
 */
object JobStream {

  val empty = toJobStream(List.empty)

  implicit def toJobStream(j: Job): JobStream = toJobStream(List(j))
  
  implicit def toJobStream(js: List[Job]): JobStream = new JobStream {
    override def produce(): Iterator[Job] = js.iterator
  }

//  def apply(t: Task): JobSequence = apply(Seq(t))

//  def apply(tasks: Seq[Task]): JobSequence = new JobSequenceTasks(tasks)

}