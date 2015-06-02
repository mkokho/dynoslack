package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._
import org.kokho.scheduling.exceptions.UnschedulableSetException

import scala.collection.mutable
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

  /*  val taskToCore: Map[Task, Int] = {
      for {
        idx <- 0.until(taskPartition.size)
        task <- taskPartition(idx)
      } yield task -> idx
    }.toMap*/


  private val localSchedules: Seq[LocalSchedule] = taskPartition.map(new LocalSchedule(_))

  val maxDeadline = if (loTasks.isEmpty) 0 else loTasks.map(_.deadline).max

  private def releaseEarlyJobs(): Unit = {
    /** Sequence of tasks that can release an early job ordered by deadline */
    /*    def getTasksForER = localSchedules
          .map(_.tasksForEarlyRelease)
          .flatten.foreach(x => {x.de})
    //      .sorted(_.deadline > _.deadline)

        */
    //To release early jobs we must be sure that there is available slack
    //First, we compute slack periods in each local schedule
    //Then, iteratively we release an early job and reduce available slack


    //if there is at least one task that can release an early job, we compute available slack
    /*    if (tasksForER.nonEmpty) {
          val availableSlack: Seq[Seq[SlackPeriod]] = localSchedules.map(_.slackForecast(maxDeadline))

          //now we compute how much slack before each moment of time
          //the map contains pairs time -> available slack
          //      val scheduleToSlack:Seq[Map[Int, Int]] = availableSlack.map()



        }*/



    localSchedules foreach (localSch => {
      //      print(s"Slack on $localSch: ")
      //      println(localSch.slackForecast(10))
    })

  }

  override def next(): Seq[ScheduledJob] = {
    releaseEarlyJobs()
    localSchedules.map(itr => itr.next())
  }

}

private class LocalSchedule(tasks_ : Set[MulticriticalTask]) extends Iterator[ScheduledJob] {

  implicit val activeJobOrdering: Ordering[ActiveJob] = Ordering.by(_.deadline)

  val tasks: mutable.Seq[MulticriticalTask] = mutable.Seq[MulticriticalTask]() ++ tasks_

  override def toString(): String = s"Schedule of $tasks"

  override def hasNext: Boolean = true

  private var time = 0
  private var activeJobs = ListBuffer[ActiveJob]()
  private var slackJobs = ListBuffer[SlackUnit]()

  /**
   * Tests whether the schedule has active jobs that are started but not finished
   *
   * @return true if there is a job that is being executed
   */
  def isBusy: Boolean = {
    if (activeJobs.isEmpty) false
    else {
      val aJob = activeJobs.head
      assert(!aJob.isCompleted) //we don't have completed job at the beginning
      assert(aJob.isBusy || activeJobs.drop(1).filter(_.isBusy).isEmpty) //there are no busy jobs if the first job is not busy
      aJob.isBusy
    }
  }

  def tasksForEarlyRelease: Seq[LoCriticalTask] = tasks collect { case loTask: LoCriticalTask if loTask.canReleaseEarlyJob(time) => loTask}

//  def extractJob(job: ActiveJob): Unit = {
//    assert(activeJobs.contains(job))
//    activeJobs -= job
//  }

  def updateTask(oldTask: LoCriticalTask, newTask: LoCriticalTask): Unit = {
    val idx = tasks.indexOf(oldTask)
    tasks(idx) = newTask
  }

  def insertJob(job: ActiveJob): Unit = {
    slackJobs.trimStart(job.length)
    cacheSlackForecast = Map.empty

    activeJobs += job
    activeJobs = activeJobs.sorted
  }

  /**
   * Keeps computed slack forecast.
   * If time does equal the first element in the tuple, we update cache
   */
  var cacheSlackForecast: Map[Int, Seq[SlackPeriod]] = Map.empty

  def slackForecast(limit: Int): Seq[SlackPeriod] = {
    if (!cacheSlackForecast.isDefinedAt(time)) {
      cacheSlackForecast = Map(time, copmutelackForecast(limit))
    }
    cacheSlackForecast(time)
  }

  def hasSlackForTask(task: LoCriticalTask): Boolean = {
    //the $slackSeq contains periods of slack.
    //we accumulate sum of slack
    def accumulateSlack(amount: Int, deadline: Int): Int = {
      val slackSeq = slackForecast(100) // change magic number 100 for actual limit
      slackSeq.foldLeft(0)((sum, period) => {
          if (period.from > deadline) sum
          else if (period.to > deadline) sum + deadline - period.from
          else sum + period.length
        })
    }

    task.execution < accumulateSlack(task.execution, time + task.deadline)
  }

  private def copmutelackForecast(limit: Int): Seq[SlackPeriod] = {
    assert(limit >= 0)

    if (limit > 0) {
      var slackSeq: List[SlackPeriod] = List()
      var copyActiveJobs = activeJobs.clone().toList
      var copySlackUnits = slackJobs.clone().toList

      def executeSlack(t: Int) = {
        slackSeq = slackSeq match {
          case Nil => List(SlackPeriod(t, t + 1))
          case head :: tail if t == head.to => SlackPeriod(head.from, t + 1) :: tail
          case _ => SlackPeriod(t, t + 1) :: slackSeq
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

      for (t <- time.until(time + limit)) {
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

      slackSeq.reverse
    } else {
      Seq.empty
    }
  }

  private def releaseJobs(time: Int): Seq[ActiveJob] = {
    //if low critical task has early released jobs its future jobs are out of period
    tasks
      .map(_.jobs(time).next())
      .filter(_.release == time)
      .map(ActiveJob)
  }


  override def next(): ScheduledJob = {

    def pushSlack(job: ActiveJob) = {
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

private case class SlackPeriod(from: Int, to: Int) {
  require(from < to)

  def length = to - from
}

