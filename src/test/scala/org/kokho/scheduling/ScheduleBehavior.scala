package org.kokho.scheduling

import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA on 6/1/2015.
 * @author: Mikhail Kokho
  */
trait ScheduleBehavior {
  this: FlatSpec =>

  def validityCheck(analyzer: ScheduleAnalyzer): Unit = {
    overdueCheck(analyzer)
    uncompletedCheck(analyzer)
  }

  def overdueCheck(analyzer: ScheduleAnalyzer): Unit = {
    assert(analyzer.findOverdueJobs().flatten.isEmpty)
  }

  def uncompletedCheck(analyzer: ScheduleAnalyzer): Unit = {
    assert(analyzer.findUncompletedJobs().flatten.isEmpty)
  }

  def aSchedule(schedule: Schedule): Unit = {
    val analyzer = new ScheduleAnalyzer(schedule)

    it should "not contain uncompleted jobs" in {
      uncompletedCheck(analyzer)
    }

    it should "not contain overdue jobs" in {
      overdueCheck(analyzer)
    }

//    it should "contain at least one job of each task" in {
//
//    }

  }

}
