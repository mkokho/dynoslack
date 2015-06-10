package org.kokho.scheduling.rts.multicritical

/**
 * Created with IntelliJ IDEA on 6/9/2015.
 * @author: Mikhail Kokho
  */
class SlackAnalyzer(val seq: Seq[SlackPeriod], val start: Int, val end: Int) {
  assert(seq.sliding(2).count(pair => pair.size == 2 && pair(0).to >= pair(1).from) == 0)

  val totalSlack = SlackPeriod.totalSlack(seq, end)

  private var _slackBehind = 0
  private var _slackAhead = totalSlack
  private var idx = 0
  private var relativeTime = 0
  private var _slackTime = List[Int]()

  private def absoluteTime = relativeTime + start

  private def slack = seq(idx)

  def slackBehind = this._slackBehind

  def slackAhead = this._slackAhead

  def slackTimes = _slackTime.reverse

  def slackUnitsBehind(t: Int):Seq[Int] = for {
    period <- seq
    unit <- period.toSlackUnits
    if unit >= start && unit < t
  } yield unit


  def slackUnitsAhead(t: Int):Seq[Int] =
    for {
      period <- seq
      unit <- period.toSlackUnits
      if unit >= t && unit < end
    } yield unit



  def isSwapAvailable = {
    idx < seq.size && absoluteTime >= slack.from && absoluteTime <= slack.to
  }


  override def toString: String = s"SState($slackBehind<-$relativeTime->$slackAhead)"

  def advanceTime(): Unit = {
    relativeTime += 1
    if (idx >= seq.size) {
      assert(_slackAhead == 0)
      assert(_slackBehind == totalSlack)
    } else if (relativeTime <= end) {
      if (absoluteTime <= slack.from) {
        //we have not entered the next slack => nothing has changed
      } else if (absoluteTime <= slack.to) {
        //we have skipped one unit of slack
        _slackBehind += 1
        _slackTime = (start+relativeTime-1) :: _slackTime
        _slackAhead -= 1
      } else {
        //we have exited the current slack period
        idx += 1
      }
    }
  }
}
