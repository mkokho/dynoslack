import java.util.concurrent.{TimeUnit, Executors}

import org.scalatest.FunSuite

import scala.concurrent.duration._
import scala.concurrent.{Promise, Future, Await}
import scala.util.Random
import scala.util.logging.Logged

/**
 * @author: Mikhail Kokho
 * @date: 7/25/2015.
 */
class ThreadOverflowTest extends FunSuite{

  def neverFuture[T]: Future[T] = {
    val p: Promise[T] = Promise()
    //we don't call failure or success because we don't want it be completed
    p.future
  }
  
  def threadOverflow(size: Int) {
    val blockTime = 4 seconds

    println(s"Creating $size threads. Each thread blocks for $blockTime")
    val schedulers = 1 to size map {x =>
      Executors.newScheduledThreadPool(1)
    }

    def task(id: Int) = new Runnable {
      override def run(): Unit = {
        Thread.sleep(blockTime.toMillis)
      }
    }

    schedulers.zipWithIndex.foreach{case (sch, i) =>
      sch.schedule(task(i), 0, TimeUnit.MILLISECONDS)
    }

    try{
      Await.ready(neverFuture, blockTime)
    }catch{
      case t: Throwable =>
        println(s"Shutting down schedulers and free threads")
        schedulers foreach {sch => sch.shutdown()}
    }
  }

  test("Creating 15 threads..."){
    threadOverflow(15)
  }

  test("15 threads has been stopped"){}

  test("Creating 100 threads..."){
    threadOverflow(100)
  }

  test("100 threads has benn stopped"){}

  /*
  test("Thread overflow. Size: 10000"){
    threadOverflow(10000)
  }

  test("Thread overflow. Size: 30000"){
    threadOverflow(30000)
  }*/
}
