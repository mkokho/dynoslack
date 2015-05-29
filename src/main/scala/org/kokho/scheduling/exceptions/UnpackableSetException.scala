package org.kokho.scheduling.exceptions

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
class UnpackableSetException(message:String, cause: Throwable) extends RuntimeException(message, cause) {
  if (cause != null)
    initCause(cause)

  def this(message: String) = this(message, null)

  def this() = this("", null)
}
