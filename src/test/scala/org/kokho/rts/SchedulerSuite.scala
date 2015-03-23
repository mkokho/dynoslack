package test.scala.org.kokho.rts

import org.scalatest.{Suite, SuiteMixin, FunSuite}
import main.scala.org.kokho.rts.standard.model._
import main.scala.org.kokho.rts.standard.impl.{BasicTask, ImmutableTaskSystem, EdfScheduler}

/**
 * @author Mikhail Kokho
 */
abstract class SchedulerSuite extends FunSuite{

  val system: TaskSystem[BasicTask]
  val scheduler: Scheduler


  test("Testing correctness of the schedule") {
    val upto = system.hyperPeriod

    val s = scheduler.schedule(system).takeWhile(_.to <= upto).toList
    val sGrouped:Map[Job, Int] = s.groupBy(_.job).mapValues(ts => ts.foldLeft(0)(_ + _.length))

    for (j <- sGrouped.keys if j != IdleJob){
      assertResult(j.execution, "units of time for " + j) {
        sGrouped(j)
      }
    }
  }
}

trait EdfScheduler extends SuiteMixin { this: Suite =>
  val scheduler = EdfScheduler
}

trait TwoTaskSystem extends SuiteMixin { this: Suite =>

  val system:TaskSystem[BasicTask] = ImmutableTaskSystem[BasicTask]() +
          BasicTask("A", 2, 5) +
          BasicTask("B", 9, 16)
}


class EdfSchedulerWithBasicTask extends SchedulerSuite
  with EdfScheduler
  with TwoTaskSystem{

}