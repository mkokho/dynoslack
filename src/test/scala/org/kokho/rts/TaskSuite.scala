package test.scala.org.kokho.rts

import org.scalatest.FunSuite
import main.scala.org.kokho.rts.model.{TaskSystem, Task}
import main.scala.org.kokho.rts.impl.{EdfScheduler, ImmutableTaskSystem, BasicTask}

import scala.reflect.runtime.universe._

/**
 * Created with IntelliJ IDEA on 11/12/14.
 * @author: Mikhail Kokho
 */
class TaskSuite extends FunSuite{

  trait TaskChecker {
    val taskA: Task
  }

  trait PeriodicTaskChecker extends TaskChecker {
    val taskA = BasicTask("A", 4, 10)
    val taskB = BasicTask("B", 3, 6)
    val taskC = BasicTask("C", 5, 15)
    val taskD = BasicTask("D", 24, 60)
    val system:TaskSystem[BasicTask] = ImmutableTaskSystem[BasicTask]() + taskA + taskB + taskC + taskD

  }


  test("Testing order of jobs in a system") {
    new PeriodicTaskChecker {
      val upto = Math.min(system.hyperPeriod, 100000)

      val jobs = system.jobs().takeWhile(_.release < upto).toList
      val valid = jobs.sortWith(_.release < _.release)

      assert(jobs.zip(valid).forall {
        case (x, y) => x.release == y.release
      }, "Order is incorrect")
    }
  }

  test("Testing whether every job is released according to its index") {
    new PeriodicTaskChecker {
      val jobs = system.jobs().takeWhile(_.release < system.hyperPeriod).
        toList.groupBy(_.toString.charAt(0))

      assert(jobs.values.forall(jList => jList.size == system.hyperPeriod / jList(0).task.period))

    }

  }



}
