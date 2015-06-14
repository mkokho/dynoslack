package org.kokho.scheduling.experiments

import org.kokho.scheduling.rts.multicritical.{LoCriticalTask, MulticriticalTask}

import scala.util.Random

/**
 * Created by Mikhail Kokho on 6/14/2015.
 */
class NotRandomTaskGenerator(val gen: RandomTaskGenerator,
                             val isGoodTask: MulticriticalTask => Boolean)
  extends TaskGenerator {

  def generateLoTask(): LoCriticalTask = {
    val task = gen.generateLoTask()
    if (isGoodTask(task)) task else generateLoTask()
  }


  def generateHiTask(): MulticriticalTask = {
    val task = gen.generateHiTask()
    if (isGoodTask(task)) task else generateHiTask()
  }

  override def generateMulticriticalTask(): MulticriticalTask = {
    if (Random.nextDouble() > gen.probHi)
      generateLoTask()
    else
      generateHiTask()
  }

}
