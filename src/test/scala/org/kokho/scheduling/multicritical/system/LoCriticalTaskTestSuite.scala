package org.kokho.scheduling.multicritical.system

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class LoCriticalTaskTestSuite
  extends FlatSpec
  with Matchers
  with LoCriticalTaskBehavior {


  def taskWithOneEarlyRelease = LoCriticalTask("L", 5, 1, List(4))

  def taskWithAnyRelease = LoCriticalTask(5, 1)


  "A low critical task (with one early release point)" should behave like aLoCriticalTask(taskWithOneEarlyRelease)

  it should behave like aLoCriticalTaskWithEarlyReleases(taskWithOneEarlyRelease)

  "A low critical task (with all possible release points)" should behave like aLoCriticalTask(taskWithAnyRelease)

  it should behave like aLoCriticalTaskWithEarlyReleases(taskWithAnyRelease)

  "A child of low critical task with one early release point" should behave like aLoCriticalTask(childTask(taskWithOneEarlyRelease))

  it should behave like aLoCriticalTaskWithEarlyReleases(childTask(taskWithOneEarlyRelease))

   "A child of a child of low critical task" should behave like aLoCriticalTask(childTask(childTask(taskWithOneEarlyRelease)))
}