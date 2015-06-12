package org.kokho.scheduling.experiments

import java.text.SimpleDateFormat
import java.util.Calendar

import org.kokho.scheduling.rts.multicritical._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, _}

/**
 * Created by Mikhail Kokho on 6/11/2015.
 */
class Experiment(val generator: TaskGenerator,
                 val utilizationBound: Double,
                 val cores: Int,
                 val repetition: Int,
                 val duration: Int,
                 val writer: java.io.Writer) {

  private def formatCurrentTime(): String = {
    val today = Calendar.getInstance().getTime
    val format = new SimpleDateFormat("MMMM dd, HH:mm:ss ")
    format.format(today)
  }

  private def output(s: String): Unit = {
    this.synchronized {
      writer.write(s)
      writer.flush()
    }
  }

  private def writeHeader(): Unit = {
    val date = formatCurrentTime()
    val genString = generator.toString
    val header =
      s"""|# Experiment on $date
          |# Parameters: $cores cores, $utilizationBound utilization bound, $duration duration, $repetition repetitions
          |# Task generator: $genString
          |
          |""".stripMargin

    output(header)

    val columns = "#  Difference is computed by subtracting first columns from the second \n" +
      "#  Idle Diff - positive means that swap scheduling is performing better \n" +
      "#  Freq Diff - negative means that swap scheduling is performing better \n" +
      "#  Idle G-ER  Idle Swap  Idle Diff   Freq G-ER  Freq Swap  Freq Diff\n\n"
    output(columns)
  }

  def output(comparison: ScheduleComparison): Unit = {
    val idleGlobal: Int = comparison.idleTimeGlobalER
    val idleSwap: Int = comparison.idleTimeSwap
    val freqGlobal: Double = comparison.frequencyImprovementGlobalER
    val freqSwap: Double = comparison.frequencyImprovementSwap
    val formattedString =
      "% 12d, % 8d, % 9d,   % 6.4f,   % 6.4f,   % 6.4f\n".format(
        idleGlobal,
        idleSwap,
        idleGlobal - idleSwap,
        freqGlobal,
        freqSwap,
        freqGlobal - freqSwap
      )

    output(formattedString)
  }

  private def doOneRun(id: Int): Future[Int] = future {
    val taskset = 0.until(cores).map(_ => generateTaskset())
    val comparison = new ScheduleComparison(taskset, duration)
    output(comparison)
    id
  }

  def run(): Future[Seq[Int]] = {
    writeHeader()
    val processes: Seq[Future[Int]] = for (x <- 1 to repetition) yield doOneRun(x)

    var remaining = repetition
    processes foreach (future => {
      future onSuccess {
        case x: Int =>
          remaining -= 1
          println(s"Run $x has completed. Remaining runs: $remaining");
      }
    })

    Future.sequence(processes)

    //    processes foreach registerSuccessCallback
    //    Thread.sleep(5000)
    //    Future.sequence(processes)

  }

  //  def statistics(sch: Schedule) = {
  //    val analyzer = new ScheduleAnalyzer(sch, 10000, true)
  //    output(analyzer.totalIdleTime + " ")
  //  }

  private def generateTaskset(): Seq[MulticriticalTask] = {
    def helper(acc: List[MulticriticalTask]): List[MulticriticalTask] = {
      val u = acc.map(_.utilization).sum
      if (u < utilizationBound - 0.05)
        helper(generator.generateMulticriticalTask() :: acc)
      else if (u < utilizationBound)
        acc
      else
        helper(acc.tail)
    }

    helper(List())
  }
}
