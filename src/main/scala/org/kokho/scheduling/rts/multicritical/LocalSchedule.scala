package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{IdleJob, ActiveJob, ScheduledJob}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */
class LocalSchedule(immutableTasks : Seq[MulticriticalTask]) extends Iterator[ScheduledJob] {

  case class SlackUnit(deadline: Int){}

  case class SlackPeriod(from: Int, to: Int) {
    require(from < to)

    def length = to - from
  }

  /**
   * For sorting sequences of active jobs
   */
  implicit val activeJobOrdering: Ordering[ActiveJob] = Ordering.by(_.deadline)

  private var time = 0
  private var activeJobs = ListBuffer[ActiveJob]()
  private var slackJobs = ListBuffer[SlackUnit]()
  private var alreadyReleasedJobs: Set[ActiveJob] = Set.empty

  private val tasks: mutable.Seq[MulticriticalTask] = mutable.Seq[MulticriticalTask]() ++ immutableTasks

  def tasksForEarlyRelease: Seq[LoCriticalTask] = tasks collect { case loTask: LoCriticalTask if loTask.canReleaseEarlyJob(time) => loTask}

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


  def releaseEarlyJob(loTask: LoCriticalTask): ActiveJob = {
    val idx = tasks.indexOf(loTask)

    require(idx != -1, s"Task $loTask is not in this schedule $this")
    require(loTask.canReleaseEarlyJob(time), s"Task $loTask cannot release an early job at time $time")

    val newTask = loTask.releaseEarlyJob(time)
    tasks(idx) = newTask

    val newJob  = ActiveJob(newTask.jobs(time).next())
    alreadyReleasedJobs = alreadyReleasedJobs + newJob

    newJob
  }


  def insertJob(job: ActiveJob): Unit = {
    slackJobs.trimStart(job.length)
    cachedSlack = Map.empty

    activeJobs += job
    activeJobs = activeJobs.sorted
  }

  /**
   * Keeps computed slack forecast.
   * If time does equal the first element in the tuple, we update cache
   */
  private var cachedSlack: Map[Int, Seq[SlackPeriod]] = Map.empty


  /**
   * Returns periods of slack from current moment of time $time to $time + $limit
   *
   * @param relativeTime how far we compute slack forecast. It is relative to the current time
   * @return sequence of slack periods ordered by increasing deadlines
   */
  def slackForecast(relativeTime: Int): Seq[SlackPeriod] = {
    val absoluteTime = time + relativeTime

    assert(cachedSlack.size <= 1)
    if (cachedSlack.isEmpty || cachedSlack.keysIterator.next() < absoluteTime){
      cachedSlack = Map(absoluteTime -> copmuteSlackForecast(relativeTime))
    }

    cachedSlack(absoluteTime)
//    copmuteSlackForecast(relativeTime)
  }

  def hasSlackForTask(task: LoCriticalTask): Boolean = {
    //the $slackSeq contains periods of slack.
    //we accumulate sum of slack
    def accumulateSlack(amount: Int, deadline: Int): Int = {
      val slackSeq = slackForecast(task.deadline) // change magic number 100 for actual limit
      slackSeq.foldLeft(0)((sum, period) => {
        if (period.from > deadline) sum
        else if (period.to > deadline) sum + deadline - period.from
        else sum + period.length
      })
    }

    val availableSlack: Int = accumulateSlack(task.execution, time + task.deadline)
    task.execution <= availableSlack
  }

  private def copmuteSlackForecast(limit: Int): Seq[SlackPeriod] = {
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
      .filterNot(alreadyReleasedJobs)

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
          case _ =>
        }
      } else {
        activeJobs.prepend(firstActiveJob)
      }

      pushSlack(firstActiveJob)

      firstActiveJob.job
    }

    time += 1
    alreadyReleasedJobs = Set.empty
    new ScheduledJob(time - 1, time, nextJob)
  }

  override def hasNext: Boolean = true

  override def toString(): String = s"Schedule of $tasks"

}
