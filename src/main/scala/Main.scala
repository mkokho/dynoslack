package main.scala

import main.scala.org.kokho.rts.model.{ActiveJob, Job}

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
object Main {

  def main(args: Array[String]) = {



//    val tau:TaskSystem = new BasicTaskSystem() +
//      BasicTask("A", 10, 25)
//      BasicTask("B", 4, 10)
//
//    println(tau)
  }






/*  def main(args: Array[String]) = {

    val tasks: Map[String, Task] = Map() +
      ("A" -> Task("A", 10, 25)) +
      ("B" -> Task("B", 4, 10)) +
      ("C" -> Task("C", 2, 16)) +
      ("D" -> Task("D", 3, 40))


    scheduleEDF(tasks.values.flatMap(_.jobs().take(10)).toList)

  }

  def scheduleEDF(jobs: List[Job])  {

    val sortedJobs = jobs.sorted(DeadlineOrder)

    var res:List[String] = List()
    def isCompleted(j: Job) = res.filter(_ == j.toString).size >= j.length

    for (t <- 0.to(sortedJobs.last.deadline)) {
      val active = sortedJobs.filter(j => t >= j.start && t < j.deadline && !isCompleted(j) )

      if (active.isEmpty)
        res ::= "Idle"
      else
        res = active.head.toString :: res

    }

    def activeLength(s: List[String]) = s.takeWhile(_ == s.head).size

    def printSchedule(t:Int, res:List[String]):String = res match {
      case Nil => t.toString
      case x :: xs => {
        val len = activeLength(res)
        val pad = Math.max(1, len / 2)

        t.toString + "".padTo(pad, '_') + x + "".padTo(pad, '_') + printSchedule(t+len, res.drop(len))
      }
    }

    println(sortedJobs)
    println(printSchedule(0, res.reverse))

  }*/

}



