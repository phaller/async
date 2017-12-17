package scala.async.internal

import java.util.{Timer, TimerTask}

import scala.concurrent.{Future, Promise, ExecutionContext}

import scala.util.Try

trait Awaitable[A] {
  def onComplete[U](f: Try[A] => U)(implicit ec: ExecutionContext): Unit
  def getCompleted: Try[A]
  def getCompletedOnly: Try[A]
}

object Awaitable {
  private val timer = new Timer(true)

  def delay(x: Long): Awaitable[Boolean] = {
    val p = Promise[Boolean]()
    timer.schedule(new TimerTask {
      def run(): Unit = p.success(true)
    }, x)
    new AwaitableFuture(p.future)
  }
}

class AwaitableFuture[A](fut: Future[A]) extends Awaitable[A] {
  def onComplete[U](f: Try[A] => U)(implicit ec: ExecutionContext): Unit = {
    fut.onComplete(f)(ec)
  }

  def getCompleted: Try[A] = {
    if (fut.isCompleted)
      fut.value.get
    else
      null
  }

  def getCompletedOnly: Try[A] = {
    if (fut.isCompleted)
      fut.value.get
    else
      null
  }
}
