package org.kokho.scheduling.multicritical.schedulers

import org.kokho.scheduling.multicritical.system.MulticriticalTask
import org.kokho.scheduling.{SchedulerAnalyzer, Job, ScheduledJob, SchedulerBehavior}
import org.scalatest.FlatSpec

/**
 * @author: Mikhail Kokho
 * @date: 6/5/15
 */
trait MulticriticalSchedulerBehavior extends SchedulerBehavior{
  this: FlatSpec =>

  type Partition = Seq[Seq[MulticriticalTask]]

  def toScheduler(tasks: Partition): MulticriticalScheduler

  val isOdd: Int => Boolean = (x: Int) => x % 2 == 1

  def dumpSchedule(analyzer:SchedulerAnalyzer, from: Int, length: Int): String = {
    def format(seq: Seq[ScheduledJob]): String =
      seq.drop(from).take(length).map(_.toString.padTo(31, " ").mkString).mkString

    analyzer.coreToJobs.map(format).mkString("\n")
  }

  def validityCheck(analyzer: SchedulerAnalyzer): Unit = {
    doubleReleaseCheck(analyzer)
    migrationCheck(analyzer)
    overdueCheck(analyzer)
    uncompletedCheck(analyzer)
  }

  def overdueCheck(analyzer: SchedulerAnalyzer): Unit = {
    val res = analyzer.findOverdueJobs().flatten
    if (res.nonEmpty) {
      val failedJob: ScheduledJob = res.head
      val debug = dumpSchedule(analyzer, failedJob.scheduledJob.release, failedJob.to - failedJob.scheduledJob.release)
      fail("There are overdue jobs. Example: " + failedJob + "\n" + debug)
    }
  }

  def uncompletedCheck(analyzer: SchedulerAnalyzer): Unit = {
    val res = analyzer.findIncorrectlyScheduled()
    if (res.nonEmpty) {
      val failedJob: Job = res.get
      val debug = dumpSchedule(analyzer, failedJob.release, failedJob.relativeDeadline)
      fail("There are uncompleted jobs. Example: " + failedJob + "\n" + debug)
    }
  }

  def migrationCheck(analyzer: SchedulerAnalyzer): Unit = {
    val res = analyzer.findMigratedJobs()
    if (res.nonEmpty) {
      val failedJob: Job = res.get
      val debug = dumpSchedule(analyzer, failedJob.release - 20, failedJob.relativeDeadline+20)
      fail(s"There is a migrated job: $res\n$debug")
    }
  }

  def doubleReleaseCheck(analyzer: SchedulerAnalyzer): Unit = {
    val res = analyzer.findDoubleReleases()
    if (res.nonEmpty) {
      val failedJob: Job = res.get
      val debug = dumpSchedule(analyzer, failedJob.release, failedJob.relativeDeadline)
      fail(s"There is a double release job: $res\n$debug")
    }
  }

  def aMulticriticalScheduler(scheduler: MulticriticalScheduler): Unit ={
    it should behave like aNonEmptyScheduler(scheduler)

    val analyzer = new SchedulerAnalyzer(scheduler)

    it should "have arity equal to the size of the partition" in {
      scheduler.arity shouldBe scheduler.partition.size
    }

    it should "not contain uncompleted jobs" in {
      uncompletedCheck(analyzer)
    }

    it should "not contain overdue jobs" in {
      overdueCheck(analyzer)
    }

    it should "not contain migrated jobs" in {
      migrationCheck(analyzer)
    }

    it should "not have double released jobs" in {
      doubleReleaseCheck(analyzer)
    }

    //    it should "contain at least one job of each task" in {
    //
    //    }

  }


  implicit def tupleToPartition(tuple: Product): Partition = {
    tuple.productIterator.toSeq.map(_.asInstanceOf[Seq[MulticriticalTask]])
  }

  implicit def tupleToScheduler(tuple: Product): MulticriticalScheduler = {
    val partition = tuple.productIterator.map(_.asInstanceOf[Seq[MulticriticalTask]])
    toScheduler(partition.toSeq)
  }

  implicit def seqToScheduler(seq: Seq[MulticriticalTask]): MulticriticalScheduler =
    toScheduler(Seq(seq))

  implicit def partitionToScheduler(partition: Partition): MulticriticalScheduler =
    toScheduler(partition)


}
