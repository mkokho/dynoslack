import java.util.concurrent.{TimeUnit, Executors}

import org.scalatest.FunSuite

import scala.concurrent.duration._
import scala.concurrent.{Promise, Future, Await}
import scala.util.Random

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
    val schedulers = 1 to size map {x =>
      Executors.newScheduledThreadPool(1)
    }

    def task(id: Int) = new Runnable {
      override def run(): Unit = {
//        println(s"Thread $id has been created. It will block forever")
        Thread.sleep(10000)
      }
    }


    schedulers.zipWithIndex.foreach{case (sch, i) =>
      sch.schedule(task(i), 0, TimeUnit.MILLISECONDS)
    }

    try{
      Await.ready(neverFuture, 4 seconds)
    }catch{
      case t: Throwable =>
        println("Shutting down schedulers, which frees threads")
        schedulers foreach {sch => sch.shutdown()}
    }
  }

  test("Thread overflow. Size: 15"){
    threadOverflow(15)
  }

/*  test("Thread overflow. Size: 1000"){
    threadOverflow(1000)
  }

  test("Thread overflow. Size: 10000"){
    threadOverflow(10000)
  }

  test("Thread overflow. Size: 30000"){
    threadOverflow(30000)
  }*/
}
