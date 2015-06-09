package org.kokho.scheduling.rts.multicritical

import org.kokho.scheduling.{ActiveJob, IdleJob, ScheduledJob, Task}

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
        val res = x.deadline - y.deadline
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
  private var swapJob: Option[SwapJob] = None

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
    if (activeJobs.isEmpty) false
    else {
      val aJob = activeJobs.head
      assert(!aJob.isCompleted) //we don't have completed job at the beginning
      assert(aJob.isBusy || activeJobs.drop(1).filter(_.isBusy).isEmpty) //there are no busy jobs if the first job is not busy
      aJob.isBusy
    }
  }

  /**
   * True if given task belongs to this schedule
   */
  def isHost(task: Task): Boolean = tasks.contains(task)

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
    assert(SlackPeriod.totalSlack(slackForecast(job.deadline), job.deadline) >= job.length, "Swap scheduling: trying to reclaim more than available")

    slackJobs.trimStart(job.length)
    cachedSlack = Map.empty

    activeJobs += job
    activeJobs = activeJobs.sorted
  }

  def insertSwapJob(job: SwapJob): Unit ={
    swapJob = Some(job)
    insertJob(new ActiveJob(job))
  }

  def extractSwapJob(): SwapJob = {
    val swapJobOption = activeJobs.find(_.job.isInstanceOf[SwapJob])

    //swap job is wrapped in an active job.
    //we delete copmleted job at the end of an iteration
    //therefore a swap job should have been completed on this processor
    assert(!swapJobOption.isDefined, "Swap job has not been completed here")

    //when inserting, we save the reference to the swap job
    //if a swap job has not been inserted, we are not able to extract it
    assert(swapJob.isDefined, "Swap job has not been inserted ")

    val j = swapJob.get
    swapJob = None
    j
  }


  /**
   * Keeps computed slack forecast.
   * If globalTime does equal the first element in the tuple, we update cache
   */
  private var cachedSlack: Map[Int, Seq[SlackPeriod]] = Map.empty


  /**
   * Returns periods of slack from current moment of globalTime $globalTime to $globalTime + $limit
   *
   * @param relativeTime how far we compute slack forecast. It is relative to the current globalTime
   * @return sequence of slack periods ordered by increasing deadlines
   */
  def slackForecast(relativeTime: Int): Seq[SlackPeriod] = {
    val absoluteTime = globalTime + relativeTime

    assert(cachedSlack.size <= 1)
    if (cachedSlack.isEmpty || cachedSlack.keysIterator.next() < absoluteTime) {
      cachedSlack = Map(absoluteTime -> copmuteSlackForecast(relativeTime))
    }

    cachedSlack(absoluteTime)
  }

  def slackDemand(task: LoCriticalTask) = if (isHost(task)) task.demand(globalTime) else task.execution


  def hasSlackForTask(task: LoCriticalTask): Boolean = {
    //the $slackSeq contains periods of slack.
    //we accumulate sum of slack

    val slackSeq = slackForecast(task.deadline)
    val availableSlack: Int = SlackPeriod.totalSlack(slackSeq, globalTime + task.deadline)
    slackDemand(task) <= availableSlack
  }

  private def copmuteSlackForecast(limit: Int): Seq[SlackPeriod] = {
    assert(limit >= 0)

    if (limit > 0) {
      var slackSeq: List[SlackPeriod] = List()
      var copyActiveJobs = activeJobs.clone().toList
      var copySlackUnits = slackJobs.clone().toList

      def executeSlack(t: Int) {
        slackSeq = slackSeq match {
          case Nil => List(SlackPeriod(t, t + 1))
          case head :: tail if t == head.to => SlackPeriod(head.from, t + 1) :: tail
          case _ => SlackPeriod(t, t + 1) :: slackSeq
        }

        if (copySlackUnits.nonEmpty)
          copySlackUnits = copySlackUnits.tail
      }

      def executeJob() {
        val newJob = copyActiveJobs.head.execute()
        if (newJob.isCompleted)
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

        (copyActiveJobs, copySlackUnits) match {
          case (Nil, Nil) => executeSlack(t)
          case (Nil, _) => executeSlack(t)
          case (jHead :: _, Nil) =>
            if (!jHead.isBusy) addSwapPoint(t)
            executeJob()
          case (jHead :: _, sHead :: _) =>
            if (sHead.deadline < jHead.deadline) {
              executeSlack(t)
            } else {
              if (!jHead.isBusy) addSwapPoint(t)
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


    val newRelease = releaseJobs(globalTime)
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

    globalTime += 1
    alreadyReleasedJobs = Set.empty
    new ScheduledJob(globalTime - 1, globalTime, nextJob)
  }

  override def hasNext: Boolean = true

  override def toString(): String = s"Schedule($globalTime) of $tasks"


  case class SlackUnit(deadline: Int) {}

}
