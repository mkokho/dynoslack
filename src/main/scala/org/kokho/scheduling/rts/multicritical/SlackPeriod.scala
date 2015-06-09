package org.kokho.scheduling.rts.multicritical

/**
 * Created by Mikhail Kokho on 6/9/2015.
 */
class SlackPeriod(val from: Int, val to: Int) {
  require(from <= to)

  def length = to - from

  override def toString: String = s"Slack($from:$to)"
}

object SlackPeriod {

  def apply(from: Int, to: Int):SlackPeriod = new SlackPeriod(from, to)

  def totalSlack(seq: Seq[SlackPeriod], end: Int): Int = totalSlack(seq, 0, end)

  private def totalSlack(seq: Seq[SlackPeriod], start: Int, end: Int): Int = {
    seq.foldLeft(0)((sum, slack) => {
//      if (slack.to <= start || slack.from >= end) sum
//      else if @TODO general case
        if (slack.from > end) sum
        else if (slack.to > end) sum + end - slack.from
        else sum + slack.length
    })
  }
}