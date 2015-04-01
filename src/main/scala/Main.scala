package main.scala

import main.scala.org.kokho.rts.standard.impl.tasks.BasicTask
import main.scala.org.kokho.rts.standard.impl.tasksystems.ImmutableTaskSystem
import main.scala.org.kokho.rts.standard.impl.EdfScheduler
import main.scala.org.kokho.rts.standard.model.{TaskSystem, Job}
import EdfScheduler.Schedule


/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
object Main {

  /** Test Case 1

Input: There are three tasks: A(5,15), B(3,7), C(4,21), where first number is the execution requirement, and the second number is the period. The EDF schedule:

  B0     A0     B1  A0   C0     B2    A1    B3   A1  C1   B4   C1
|_ _ _|_ _ _ _|_ _ _|_|_ _ _ _|_ _ _|_ _ _|_ _ _|_ _|_ _|_ _ _|_ _
0 . 2 . 4 . 6 . 8 . 10  2 . 4 . 6 . 8 . 20  2 . 4 . 6 . 8 . 30

Assume that all jobs of A takes 2 units of time instead of 5. EDF schedule with slack

  B0     A0     B1  A0   C0
|_ _ _|_ _ _ _|_ _ _|_|_ _ _ _|
0 . 2 . 4 . 6 . 8 . 10  2 . 4 .

Output: we run SlackAvailability(C_1, 30) at different moments of time. Recall that there if there is a preempted job at time t, then (t,x) is not a swap point.

    at time 3 it returns no swap points
    at time 5 it returns (5,0), (6,1),(7,2),(10,2),(11,3),(15,3),(18.3),(26,3).
    at time 6 the schedule is changed since we use slack for C0. C0 generates slack (3,21), which is placed in [12,15]
              swap points: (12,0), (13,1), (14,2), (15,3),(18.3),(26,3).

  B0   A0  C0   B1   C0
|_ _ _|_ _|_ _|_ _ _|_ _|_ _ _|
0 . 2 . 4 . 6 . 8 . 10  2 . 4 .

    */

  private def testCase1:Schedule = {

    val system:TaskSystem[BasicTask] = ImmutableTaskSystem[BasicTask]() +
      //      BasicTask("A", 18, 32) +
      //      BasicTask("B", 4, 10)
      BasicTask("A", 5, 15) +
      BasicTask("B", 3, 7) +
      BasicTask("C", 4, 21)

    EdfScheduler.schedule(system)
  }



  private def testCase2 = {
    val system1 = ImmutableTaskSystem[BasicTask]() +
      BasicTask("A", 5, 11) +
      BasicTask("B", 3, 12) +
      BasicTask("C", 4, 14)

    val system2 = ImmutableTaskSystem[BasicTask]() +
      BasicTask("D", 6, 11) +
      BasicTask("X", 4, 10)

    List(system1, system2).map(EdfScheduler.schedule)
  }

  def main(args: Array[String]) = {

//    val schedule = testCase1
    val schedule = testCase2


    println(EdfScheduler.drawSchedule(schedule, 0, 22))
  }


}



