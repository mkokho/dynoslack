package main.scala

import main.scala.org.kokho.rts.standard.impl.{EdfScheduler, ImmutableTaskSystem, BasicTask}
import main.scala.org.kokho.rts.standard.model.{TaskSystem, ActiveJob, Job}

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
object Main {

  def main(args: Array[String]) = {

    val system:TaskSystem[BasicTask] = ImmutableTaskSystem[BasicTask]() +
      BasicTask("A", 4, 9) +
      BasicTask("B", 3, 6)


    print(EdfScheduler.drawSchedule(EdfScheduler.schedule(system), 50))
  }


}



