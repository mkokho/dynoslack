package main.scala.org.kokho.rts.standard.model

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
trait Scheduler {

  type Schedule = Iterator[Allocation]

  def schedule[T <: Task](ts: TaskSystem[T]):Schedule

  def drawSchedule(sList:List[Schedule], limit: Int):String = sList.map(drawSchedule(_, limit)).mkString("\n\n")

  def drawSchedule(sList:List[Schedule], from:Int, to: Int):String = sList.map(drawSchedule(_, from, to)).mkString("\n\n")

  def drawSchedule(s: Schedule, to: Int):String = drawSchedule(s, 0, to)


  def drawSchedule(s: Schedule, from:Int, to: Int):String = {
    val allocation = s.takeWhile(_.from < to).toList


    val sbJobs = new StringBuilder(2*to)
    val sbIntervals = new StringBuilder(2*to)
    val sbTicks = new StringBuilder(2*to)

    for (a <- allocation) {
      val name = a.job.toString
      sbJobs ++= " " * (a.length - 1)
      sbJobs ++= name
      sbJobs ++= " " * (a.length + 1 - name.length)
    }

    for (a <- allocation){
      if (a.from == a.job.release)
        sbIntervals ++= "↓_"
      else
        sbIntervals ++= "|_"
      sbIntervals ++= " _"  * (a.length - 1)
    }

    for (t <- Range(0, to, 1)){
      if (t % 10 == 0 && t != 0){
        if (t >= 100) {
          sbTicks ++= t.toString.drop(1)
        }else{
          sbTicks ++= t.toString
        }
      }else if (t % 10 == 1 && t != 1){
        sbTicks ++= "  "
      } else{
        if (t % 2 == 0) {
          sbTicks ++= (t % 10).toString
          sbTicks += ' '
        }else
          sbTicks ++= ". "
      }
    }

//    res ++= "\n" + allocation.mkString(", ")

    List(sbJobs, sbIntervals, sbTicks).map(_.toString().take(2*to).drop(2*from)).mkString("\n")
  }

}

case class Allocation(from: Int, to: Int, job: Job) {
  require(to > from, "Allocation must have 'from' greater than 'to'")

  def length = to - from

  override def toString: String = job + "->" + from + ":" + to
}
