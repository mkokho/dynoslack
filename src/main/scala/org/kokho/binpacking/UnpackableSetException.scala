package org.kokho.binpacking

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class UnpackableSetException(message:String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
}
