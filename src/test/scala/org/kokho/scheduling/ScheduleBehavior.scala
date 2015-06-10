package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/1/2015.
 * @author: Mikhail Kokho
 */
trait ScheduleBehavior {
  this: FlatSpec =>

  def validityCheck(analyzer: ScheduleAnalyzer): Unit = {
    doubleReleaseCheck(analyzer)
    migrationCheck(analyzer)
    overdueCheck(analyzer)
    uncompletedCheck(analyzer)
  }

  def overdueCheck(analyzer: ScheduleAnalyzer): Unit = {
    val res = analyzer.findOverdueJobs().flatten
    if (res.nonEmpty) {
      val failedJob: ScheduledJob = res.head
      val debug = analyzer.debugInfo(failedJob.job.release, failedJob.to - failedJob.job.release)
      fail("There are overdue jobs. Example: " + failedJob + "\n" + debug)
    }
  }

  def uncompletedCheck(analyzer: ScheduleAnalyzer): Unit = {
    val res = analyzer.findUncompletedJobs().flatten
    if (res.nonEmpty) {
      val failedJob: Job = res.head
      val debug = analyzer.debugInfo(failedJob.release, failedJob.relativeDeadline)
      fail("There are uncompleted jobs. Example: " + failedJob + "\n" + debug)
    }
  }

  def migrationCheck(analyzer: ScheduleAnalyzer): Unit = {
    val res = analyzer.findMigratedJobs()
    if (res.nonEmpty) {
      val failedJob: Job = res.get
      val debug = analyzer.debugInfo(failedJob.release - 20, failedJob.relativeDeadline+20)
      fail(s"There is a migrated job: $res\n$debug")
    }
  }

  def doubleReleaseCheck(analyzer: ScheduleAnalyzer): Unit = {
    val res = analyzer.findDoubleReleases()
    if (res.nonEmpty) {
      val failedJob: Job = res.get
      val debug = analyzer.debugInfo(failedJob.release, failedJob.relativeDeadline)
      fail(s"There is a double release job: $res\n$debug")
    }
  }


  def aSchedule(schedule: Schedule): Unit = {
    val analyzer = new ScheduleAnalyzer(schedule)

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

}
