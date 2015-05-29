package org.kokho.scheduling

/**
 * Created with IntelliJ IDEA on 5/28/15.
 * @author: Mikhail Kokho
 */
trait Core {

}

/**
 * Represents cores with identical speed
 */
final case class UniformCore(idx: Int) extends Core{

}
