package org.kokho.scheduling

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
  def produce(): Iterator[Job] = produce(0)


  /**
   * Returns jobs that are released after or at $from in order of their release times
   */
  def produce(from: Int): Iterator[Job]

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
    override def produce(): Iterator[Job] = produce(0)

    override def produce(from: Int): Iterator[Job] = {
      val jobsThat = that.produce(from).buffered
      val jobsSelf = self.produce(from).buffered
      mergeJobs(jobsSelf, jobsThat)
    }
  }
  
  def remove(removedJob: Job): JobStream = new JobStream {
    override def produce(from: Int): Iterator[Job] = self.produce(from).filter(_ != removedJob)
  }

  /**
   * Inserts a job into this job sequence
   */
  def insert(job: Job): JobStream = merge(job)


  private def mergeJobs(xs: BufferedIterator[Job], ys: BufferedIterator[Job]) = new Iterator[Job] {
    override def hasNext: Boolean = xs.nonEmpty || ys.nonEmpty

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

  val empty = new JobStream {
    override def produce(from: Int): Iterator[Job] = Iterator.empty
  }

  implicit def toJobStream(j: Job): JobStream = toJobStream(List(j))
  
  implicit def toJobStream(js: List[Job]): JobStream = new JobStream {
    override def produce(from: Int): Iterator[Job] = js.dropWhile(_.release < from).iterator
  }

  implicit def toJobStream(ts: Seq[Task]): JobStream = ts.map(toJobStream).reduce(_ merge _)

  implicit def toJobStream(task: Task): JobStream = new JobStream {
    override def produce(from: Int): Iterator[Job] = task.jobs(from)
  }

}