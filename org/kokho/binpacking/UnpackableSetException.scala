package org.kokho.binpacking

/**
 * @author: Mikhail Kokho
 * @date: 5/28/15
 */
class UnpackableSetException(message:String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
}
