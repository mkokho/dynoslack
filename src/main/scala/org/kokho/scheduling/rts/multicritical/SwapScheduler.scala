package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._
import org.kokho.scheduling.exceptions.UnschedulableSetException

import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class SwapScheduler() extends Scheduler {

  override type AllowedTasks = MulticriticalTask

  override def schedule(partition: Seq[Set[MulticriticalTask]]): Schedule = {
    val bigSets = partition.filter(_.map(_.utilization).sum > 1)
    if (bigSets.size > 0) {
      throw new UnschedulableSetException(
        s"Cannot schedule sets of tasks with total utilization > 1: $bigSets"
      )
    }

    new SwapSchedule(partition)
  }
}


final class SwapSchedule(val taskPartition: Seq[Set[MulticriticalTask]]) extends Schedule {

  override def partition: Seq[Seq[Task]] = {
    def toTask(x: MulticriticalTask): Task = x.asInstanceOf[Task]
    taskPartition.map(_.map(toTask).toSeq)
  }

  val loTasks: Seq[LoCriticalTask] = tasks collect { case task: LoCriticalTask => task}
  val hiTasks: Seq[HiCriticalTask] = tasks collect { case task: HiCriticalTask => task}

  if (tasks.size != loTasks.size + hiTasks.size) {
    val unsupportedClasses = tasks.diff(loTasks).diff(hiTasks).map(_.getClass.getCanonicalName)
    throw new IllegalArgumentException(
      "Only tasks of LoCriticalTask or HiCriticalTasks classes are supported" +
        s"Given classes: $unsupportedClasses"
    )
  }

  val taskToCore: Map[Task, Int] = {
    for {
      idx <- 0.until(taskPartition.size)
      task <- taskPartition(idx)
    } yield task -> idx
  }.toMap

  //  private def toJobsIterator(tasks: Set[MulticriticalTask]): Iterator[Job] = new LocalSchedule(tasks)

  private val iterators: Seq[LocalSchedule] = taskPartition.map(new LocalSchedule(_))

  override def next(): Seq[ScheduledJob] = {
    iterators.map(itr => itr.next())
  }

}

private class LocalSchedule(tasks: Set[MulticriticalTask]) extends Iterator[ScheduledJob] {
  override def hasNext: Boolean = true

  implicit val activeJobOrdering: Ordering[ActiveJob] = Ordering.by(_.deadline)

  private var time = 0
  private var activeJobs = ListBuffer[ActiveJob]()
  private var slackJobs = ListBuffer[SlackUnit]()

  def slackForecast(limit: Int): Seq[SlackPeriod] = {
    assert(limit > time)

    var slackSeq: List[SlackPeriod] = List()
    var copyActiveJobs = activeJobs.clone().toList
    var copySlackUnits = slackJobs.clone().toList

    def executeSlack(t: Int) = {
      slackSeq = slackSeq match {
        case Nil => List(SlackPeriod(t, t+1))
        case head :: tail if t == head.to => SlackPeriod(head.from, t + 1) :: tail
        case _ =>  SlackPeriod(t, t+1) :: slackSeq
      }

      if (copySlackUnits.nonEmpty)
        copySlackUnits = copySlackUnits.tail
    }

    def executeJob() = {
      val newJob = copyActiveJobs.head.execute()
      if (newJob.isCompleted)
        copyActiveJobs = copyActiveJobs.tail
      else
        copyActiveJobs = newJob :: copyActiveJobs.tail
    }

    for (t <- time.until(limit)) {
      copyActiveJobs = (copyActiveJobs ++ releaseJobs(t)).sorted

      (copyActiveJobs, copySlackUnits) match {
        case (Nil, Nil) => executeSlack(t)
        case (Nil, _) => executeSlack(t)
        case (_, Nil) => executeJob()
        case (jHead :: _, sHead :: _) =>
          if (sHead.deadline < jHead.deadline) {
            executeSlack(t)
          } else {
            executeJob()
          }
      }
    }

    slackSeq
  }

  private def releaseJobs(time: Int): Set[ActiveJob] = {
    tasks
      .filter(time % _.period == 0)
      .map(_.jobs(time).next())
      .map(ActiveJob)
  }

  private def pushSlack(job: ActiveJob) = {
    if (slackJobs.nonEmpty && slackJobs.head.deadline < job.deadline) {
      slackJobs.trimStart(1)
      slackJobs.append(new SlackUnit(job.deadline))
    }
  }

  def generateSlack(hiJob: HiCriticalJob) = {
    assert(hiJob.takeLowWcet)
    val len = hiJob.hiWcet - hiJob.loWcet
    val units = 1.to(len).map(_ => new SlackUnit(hiJob.deadline))
    slackJobs.prependAll(units)
    slackJobs = slackJobs.sortWith(_.deadline < _.deadline)
  }

  override def next(): ScheduledJob = {
    if (slackJobs.nonEmpty || activeJobs.isEmpty) {
      println(slackForecast(time+10))
    }

    val newRelease = releaseJobs(time)
    if (newRelease.nonEmpty) {
      activeJobs ++= newRelease
      activeJobs = activeJobs.sorted
    }

    val nextJob = if (activeJobs.isEmpty) {
      if (slackJobs.nonEmpty) slackJobs.trimStart(1)
      IdleJob
    } else {
      val firstActiveJob = activeJobs.head.execute()
      activeJobs.trimStart(1)

      if (firstActiveJob.isCompleted) {
        firstActiveJob.job match {
          case hiJob: HiCriticalJob => if (hiJob.takeLowWcet) generateSlack(hiJob)
        }
      } else {
        activeJobs.prepend(firstActiveJob)
      }

      pushSlack(firstActiveJob)

      firstActiveJob.job
    }

    time += 1
    new ScheduledJob(time - 1, time, nextJob)
  }
}

private class SlackUnit(val deadline: Int)

private case class SlackPeriod(val from: Int, val to: Int) {
  require(from < to)
}

