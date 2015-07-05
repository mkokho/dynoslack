package org.kokho.scheduling

import org.kokho.scheduling.multicritical.schedulers.SchedulerWithLocalER
import org.kokho.scheduling.multicritical.system.{LoCriticalTask, HiCriticalTask}
import org.scalatest.{Matchers, FunSuite}

/**
 * @author: Mikhail Kokho
 * @date: 7/4/2015.
 */
class SchedulerAnalyzerTests extends FunSuite with Matchers{

  val taskA = HiCriticalTask(10, 4, 4)
  val taskC = LoCriticalTask(10, 4, List(9))

  val simpleSet = Seq(Seq(taskA, taskC))
  val scheduler = new SchedulerWithLocalER(simpleSet)
  val analyzer = new SchedulerAnalyzer(scheduler, 11)

  test("find jobs released by a task") {
    analyzer.findJobs(taskA).forall(_.scheduledJob.isOfTask(taskA)) shouldBe true
  }

  test("merges scheduled jobs") {
    val jobA = Job(0, 4, 10)
    val jobB = Job(0, 4, 10)
    val seq = Seq() :+
      ScheduledJob(0,1, jobA) :+
      ScheduledJob(1,2,jobA) :+
      ScheduledJob(2,3,jobB) :+
      ScheduledJob(3,4,jobA) :+
      ScheduledJob(4,5,jobB) :+
      ScheduledJob(5,6,jobB)

    val expected =  Seq() :+
      ScheduledJob(0,2, jobA) :+
      ScheduledJob(2,3,jobB) :+
      ScheduledJob(3,4,jobA) :+
      ScheduledJob(4,6,jobB)

    analyzer.mergeScheduledJobs(seq) shouldEqual expected

  }

  test("computes number of early released jobs") {
    analyzer.countEarlyReleases(taskC) shouldBe 1
  }

  test("computes idle time correctly") {
    analyzer.totalIdleTime shouldBe 1
  }

  test("finds uncompleted jobs") {
    val job = Job(0,3,5)
    val jobUnit = Job(0,1,5)
    val scheduleUncomplete = Seq(
      Seq() :+ ScheduledJob(0,1,job) :+ ScheduledJob(0,1,jobUnit),
      Seq() :+ ScheduledJob(1,2,IdleJob) :+ ScheduledJob(1,2,job))

    val scheduleCopmlete = scheduleUncomplete :+ Seq(ScheduledJob(2,3,IdleJob),ScheduledJob(2,3,job))

    assertResult(Some(job)) {
      new SchedulerAnalyzer(new SchedulerFixed(scheduleUncomplete)).findIncorrectlyScheduled()
    }

    assertResult(None) {
      new SchedulerAnalyzer(new SchedulerFixed(scheduleCopmlete)).findIncorrectlyScheduled()
    }

  }

  test("finds migrated jobs") {
    val job = Job(0,2,5)
    val job2 = Job(0,1,5)

    assertResult(Some(job)) {
      val migratedSet = Seq() :+
        Seq(ScheduledJob(0,1,job), ScheduledJob(0,1,IdleJob)) :+
        Seq(ScheduledJob(1,2,job2), ScheduledJob(1,2,job))

      new SchedulerAnalyzer(new SchedulerFixed(migratedSet)).findMigratedJobs()
    }

    assertResult(None) {
      val nonMigratedSet = Seq() :+
        Seq(ScheduledJob(0, 1, job)) :+
        Seq(ScheduledJob(0,1,job2))
      new SchedulerAnalyzer(new SchedulerFixed(nonMigratedSet)).findMigratedJobs()
    }
  }

  test("finds double releases") {
    val task = HiCriticalTask(4,2,2)
    val job1 = task.job(0)
    val job2 = task.job(1)

    assertResult(Some(job2)) {
      val badSchedule = Seq() :+
        Seq(ScheduledJob(0,1,job1), ScheduledJob(0,1,IdleJob)) :+
        Seq(ScheduledJob(1,2,job1), ScheduledJob(1,2,job2))

      new SchedulerAnalyzer(new SchedulerFixed(badSchedule)).findDoubleReleases()
    }

    assertResult(None) {
      val goodSchedule = Seq() :+
        Seq(ScheduledJob(0,1,job1), ScheduledJob(0,1,IdleJob)) :+
        Seq(ScheduledJob(1,2,job1), ScheduledJob(1,2,IdleJob))
        Seq(ScheduledJob(2,3,IdleJob), ScheduledJob(2,3,job2))

      new SchedulerAnalyzer(new SchedulerFixed(goodSchedule)).findDoubleReleases()
    }
  }
}

class SchedulerFixed(fixedSchedule: Seq[Seq[ScheduledJob]]) extends Scheduler {
  override type AllowedTasks = Task

  override def arity: Int = fixedSchedule.size

  /**
   * Infinite iterator over a sequence of scheduled jobs
   */
  override def iterate(): Iterator[Seq[ScheduledJob]] = fixedSchedule.iterator

  /**
   * Tasks that are being scheduled
   */
  override def tasks: Seq[Task] = fixedSchedule
    .flatten
    .flatMap(_.scheduledJob.releasedBy)
}
