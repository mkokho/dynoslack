package org.kokho.scheduling.rts.multicritical

/**
 * Created with IntelliJ IDEA on 6/9/2015.
 * @author: Mikhail Kokho
  */
class SlackPeriod(val from: Int, val to: Int) {
  require(from <= to)

  def length = to - from

  def toSlackUnits: Seq[Int] = from.until(to)

  override def toString: String = s"Slack($from:$to)"
}

object SlackPeriod {

  def apply(from: Int, to: Int):SlackPeriod = new SlackPeriod(from, to)

  def totalSlack(seq: Seq[SlackPeriod], end: Int): Int = totalSlack(seq, 0, end)

  def totalSlack(seq: Seq[SlackPeriod], start: Int, end: Int): Int = {
    seq.foldLeft(0)((sum, slack) =>
        if (start >= slack.to) sum
        else if (slack.from <= start) sum + slack.to - start
        else if (slack.from >= end) sum
        else if (slack.to >= end) sum + end - slack.from
        else sum + slack.length
    )
  }
}