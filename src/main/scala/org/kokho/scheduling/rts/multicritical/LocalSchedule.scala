package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA on 6/3/15.
 * @author: Mikhail Kokho
 */
class LocalSchedule(immutableTasks: Seq[MulticriticalTask]) extends Iterator[ScheduledJob] {

  /**
   * For sorting sequences of active jobs
   */
  implicit val activeJobOrdering: Ordering[ActiveJob] = //Ordering.by(_.deadline)
    new Ordering[ActiveJob] {
      override def compare(x: ActiveJob, y: ActiveJob): Int = {
        val res = x.job.deadline - y.job.deadline
        if (res == 0) {
          (x.job, y.job) match {
            case (_: LoCriticalJob, _: HiCriticalJob) => 1
            case (_: HiCriticalJob, _: LoCriticalJob) => -1
            case _ => 0
          }
        } else
          res
      }
    }

  private var globalTime = 0

  private var activeJobs = ListBuffer[ActiveJob]()
  private var slackJobs = ListBuffer[SlackUnit]()
  private var globalSwapJob: Option[SwapJob] = None

  private var alreadyReleasedJobs: Set[ActiveJob] = Set.empty

  private val tasks: mutable.Seq[MulticriticalTask] = mutable.Seq[MulticriticalTask]() ++ immutableTasks

  def tasksForEarlyRelease: Seq[LoCriticalTask] =
    tasks collect {
      case loTask: LoCriticalTask
        if loTask.canReleaseEarlyJob(globalTime) && !isActive(loTask) => loTask
    }

  /**
   * Tests whether the schedule has active jobs that are started but not finished
   *
   * @return true if there is a job that is being executed
   */
  def isBusy: Boolean = {
    if (globalSwapJob.isDefined) true
    else if (activeJobs.isEmpty) false
    else {
      activeJobs.exists(_.isBusy)
      //      val aJob = activeJobs.head
      //      assert(!aJob.isCompleted) //we don't have completed job at the beginning
      //      assert(aJob.isBusy || activeJobs.drop(1).filter(_.isBusy).isEmpty) //there are no busy jobs if the first job is not busy
      //      aJob.isBusy
    }
  }

  /**
   * True if given task belongs to this schedule
   */
  def isHost(task: Task): Boolean = tasks.contains(task)

  def isSwapActive(): Boolean = globalSwapJob.isDefined

  def isSwapActive(task: Task): Boolean =
    globalSwapJob.isDefined && globalSwapJob.get.job.isOfTask(task)

  /**
   * True if there is active job of the given task
   */
  private def isActive(task: MulticriticalTask) =
    activeJobs.count(_.job.releasedBy.get == task) > 0


  def releaseEarlyJob(loTask: LoCriticalTask): ActiveJob = {
    val idx = tasks.indexOf(loTask)

    require(idx != -1, s"Task $loTask is not in this schedule $this")
    require(loTask.canReleaseEarlyJob(globalTime), s"Task $loTask cannot release an early job at globalTime $globalTime")

    val newTask = loTask.shiftedTasks(globalTime)
    tasks(idx) = newTask

    val newJob = ActiveJob(newTask.jobs(globalTime).next())
    alreadyReleasedJobs = alreadyReleasedJobs + newJob

    newJob
  }


  def insertJob(job: ActiveJob): Unit = {
    //slack jobs are those generated at runtime
    //there is also static slack, that will appear later
    //we consume as much dynamic slack as we have
    //@TODO we might consume too much, is this schedule is host for the given task
    //    assert(SlackPeriod.totalSlack(slackForecast(job.job.deadline), job.job.deadline) >= job.job, s"Job $job is trying to reclaim more than available")

    slackJobs.trimStart(job.length)
    cachedSlack = None

    activeJobs += job
    activeJobs = activeJobs.sorted
  }


  def insertSwapJob(job: SwapJob): Unit = {
    assert(globalSwapJob.isEmpty, "The schedule already has a swap job")
    assert(job.executionPlan.forall(_ >= globalTime), "Trying to insert late swap job")
    globalSwapJob = Some(job)
    cachedSlack = None
    slackJobs.trimStart(job.executionPlan.length)
  }

  /**
   * Keeps computed slack forecast.
   * If globalTime does equal the first element in the tuple, we update cache
   */
  private var cachedSlack: Option[(Int, Seq[SlackPeriod])] = None


  /**
   * Returns periods of slack from current moment of globalTime $globalTime to $globalTime + $limit
   *
   * @param relativeTime how far we compute slack forecast. It is relative to the current globalTime
   * @return sequence of slack periods ordered by increasing deadlines
   */
  def slackForecast(relativeTime: Int): Seq[SlackPeriod] = {
    val absoluteTime = globalTime + relativeTime

    if (cachedSlack.isEmpty || cachedSlack.get._1 < absoluteTime) {
      cachedSlack = Some(absoluteTime, copmuteSlackForecast(relativeTime))
    }

    cachedSlack.get._2
  }

  def slackDemand(task: LoCriticalTask) = globalSwapJob match {
    case None => if (isHost(task)) task.demand(globalTime) else task.execution
    case Some(SwapJob(_, swapTime, _, _)) =>
      if (isHost(task) && globalTime + task.period < swapTime) task.demand(globalTime)
      else task.execution
  }


  def hasSlackForTask(task: LoCriticalTask): Boolean = {
    val slackSeq = slackForecast(task.deadline)
    val availableSlack: Int = SlackPeriod.totalSlack(slackSeq, globalTime + task.deadline)
    slackDemand(task) <= availableSlack
  }

  private var busyTimes: List[Int] = List[Int]()

  def isFutureBusy(t: Int) = busyTimes.contains(t)

  private def copmuteSlackForecast(limit: Int): Seq[SlackPeriod] = {
    assert(limit >= 0)

    if (limit > 0) {
      var slackSeq: List[SlackPeriod] = List()
      var copyActiveJobs = activeJobs.clone().toList
      var copySlackUnits = slackJobs.clone().toList
      busyTimes = List()

      def executeSlack(t: Int) {
        //swap job also takes slack
        if (!globalSwapJob.exists(_.executionPlan.contains(t))) {
          slackSeq = slackSeq match {
            case Nil => List(SlackPeriod(t, t + 1))
            case head :: tail if t == head.to => SlackPeriod(head.from, t + 1) :: tail
            case _ => SlackPeriod(t, t + 1) :: slackSeq
          }

          if (copySlackUnits.nonEmpty)
            copySlackUnits = copySlackUnits.tail
        }
      }

      def executeJob() {
        val newJob = copyActiveJobs.head.execute()
        val completed = newJob.job match {
          case j: HiCriticalJob => newJob.elapsedTime >= j.hiWcet
          case _ => newJob.isCompleted
        }
        if (completed)
          copyActiveJobs = copyActiveJobs.tail
        else
          copyActiveJobs = newJob :: copyActiveJobs.tail
      }

      def addSwapPoint(t: Int) {
        if (slackSeq.isEmpty || slackSeq.head.to != t)
          slackSeq = SlackPeriod(t, t) :: slackSeq
      }

      for (t <- globalTime.until(globalTime + limit)) {
        copyActiveJobs = (copyActiveJobs ++ releaseJobs(t)).sorted
        if (copyActiveJobs.exists(_.isBusy)) busyTimes = t :: busyTimes

        (copyActiveJobs, copySlackUnits) match {
          case (Nil, Nil) => executeSlack(t)
          case (Nil, _) => executeSlack(t)
          case (jHead :: _, Nil) =>
            if (!copyActiveJobs.exists(_.isBusy)) addSwapPoint(t)
            executeJob()
          case (jHead :: _, sHead :: _) =>
            if (sHead.deadline < jHead.job.deadline) {
              executeSlack(t)
            } else {
              if (!copyActiveJobs.exists(_.isBusy)) addSwapPoint(t)
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

  private def pushSlack(job: Job) = {
    if (slackJobs.nonEmpty && slackJobs.head.deadline < job.deadline) {
      slackJobs.trimStart(1)
      slackJobs.append(new SlackUnit(job.deadline))
    }
  }

  private def generateSlack(hiJob: HiCriticalJob) = {
    assert(hiJob.takeLowWcet)
    val len = hiJob.hiWcet - hiJob.loWcet
    val units = 1.to(len).map(_ => new SlackUnit(hiJob.deadline))
    slackJobs.prependAll(units)
    slackJobs = slackJobs.sortWith(_.deadline < _.deadline)
  }


  private def executeActiveJob(): Job = {
    assert(globalSwapJob.isEmpty || globalSwapJob.get.executionPlan.forall(_ > globalTime),
      "It must be swap job for executing")

    val noSlackPush = globalSwapJob.isDefined

    if (activeJobs.isEmpty) {
      if (slackJobs.nonEmpty) slackJobs.trimStart(1)
      IdleJob
    } else {
      if (noSlackPush && slackJobs.nonEmpty && slackJobs.head.deadline < activeJobs.head.job.deadline) {
        slackJobs.trimStart(1)
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

        pushSlack(firstActiveJob.job)
        firstActiveJob.job
      }
    }
  }

  private def executeWithSwapJob(swapJob: SwapJob): Job = {
    //swap job has higner priority than any job
    //but also swap job is executed according to the precomputed plan
    //therefore if its not time to execute swap job, we execute active job

    assert(!swapJob.isComplete, "Completed swap job should be dropped")
    assert(swapJob.executionPlan.forall(_ >= globalTime), "Swap job missed its execution plan")
    if (swapJob.executionPlan.head == globalTime) {
      val nextSwapJob = swapJob.execute(globalTime)
      if (!nextSwapJob.isComplete)
        globalSwapJob = Some(nextSwapJob)
      else
        globalSwapJob = None

      nextSwapJob.job
    } else {
      executeActiveJob()
    }
  }

  override def next(): ScheduledJob = {

    val newRelease = releaseJobs(globalTime)
    if (newRelease.nonEmpty) {
      activeJobs ++= newRelease
      activeJobs = activeJobs.sorted
    }

    val nextJob = globalSwapJob match {
      case None => executeActiveJob()
      case Some(sj) => executeWithSwapJob(sj)
    }

    globalTime += 1
    alreadyReleasedJobs = Set.empty
    new ScheduledJob(globalTime - 1, globalTime, nextJob)
  }

  override def hasNext: Boolean = true

  override def toString(): String = s"Schedule($globalTime) of $tasks"


  case class SlackUnit(deadline: Int) {}

}
