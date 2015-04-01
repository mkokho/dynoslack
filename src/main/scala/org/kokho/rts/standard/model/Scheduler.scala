package main.scala.org.kokho.rts.standard.model

/**
 * Created with IntelliJ IDEA on 11/11/14.
 * @author: Mikhail Kokho
 */
trait Scheduler {

  type Schedule = Iterator[Allocation]

  def schedule[T <: Task](ts: TaskSystem[T]):Schedule

  def drawSchedule(sList:List[Schedule], limit: Int):String = sList.map(drawSchedule(_, limit)).mkString("\n\n")

  def drawSchedule(s: Schedule, limit: Int):String = {
    val allocation = s.takeWhile(_.from < limit).toList

    val res = new StringBuilder(limit*2+3)
    for (a <- allocation) {
      val name = a.job.toString
      res ++= " " * (a.length - 1)
      res ++= name
      res ++= " " * (a.length + 1 - name.length)
    }
    res += '\n'

    for (a <- allocation){
      if (a.from == a.job.release)
        res ++= "↓_"
      else
        res ++= "|_"
      res ++= " _"  * (a.length - 1)
    }
    res += '\n'

    for (t <- Range(0, limit, 1)){
      if (t % 10 == 0 && t != 0){
        res ++= t.toString
      }else if (t % 10 == 1 && t != 1){
        res ++= "  "
      } else{
        if (t % 2 == 0) {
          res ++= (t % 10).toString
          res += ' '
        }else
          res ++= ". "
      }
    }

//    res ++= "\n" + allocation.mkString(", ")

    res.toString()
  }

}

case class Allocation(from: Int, to: Int, job: Job) {
  require(to > from, "Allocation must have from greater than to")

  def length = to - from

  override def toString: String = job + "->" + from + ":" + to
}
