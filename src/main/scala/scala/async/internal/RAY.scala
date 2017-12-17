package scala.async.internal

import language.experimental.macros

import scala.reflect.macros.Context

import scala.concurrent._

object RAY extends AsyncBase {
  lazy val futureSystem = RAYFutureSystem
  type FS = RAYFutureSystem.type

  def async[T](body: => T)(implicit execContext: ExecutionContext): Future[T] = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T])(execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = super.asyncImpl[T](c)(body)(execContext)
}

object RAYFutureSystem extends FutureSystem {

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext
  type Tryy[A] = scala.util.Try[A]

  type Awaiter[A] = Awaitable[A]

  def mkOps(c0: Context): Ops {val c: c0.type} = new Ops {
    val c: c0.type = c0
    import c.universe._

    def promType[A: WeakTypeTag]: Type = weakTypeOf[Promise[A]]
    def tryType[A: WeakTypeTag]: Type = weakTypeOf[scala.util.Try[A]]
    def execContextType: Type = weakTypeOf[ExecutionContext]

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = reify {
      Promise[A]()
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = reify {
      prom.splice.future
    }

    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]) = reify {
      Future(a.splice)(execContext.splice)
    }

    override def continueCompletedFutureOnSameThread: Boolean = true

    def onComplete[A, U](awaiter: Expr[Awaiter[A]], fun: Expr[Tryy[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      awaiter.splice.onComplete(fun.splice)(execContext.splice)
    }

    override def getCompleted[A: WeakTypeTag](awaiter: Expr[Awaiter[A]]): Expr[Tryy[A]] = reify {
      awaiter.splice.getCompleted
    }

    override def getCompletedOnly[A: WeakTypeTag](awaiter: Expr[Awaiter[A]]): Expr[Tryy[A]] = reify {
      awaiter.splice.getCompletedOnly
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[Tryy[A]]): Expr[Unit] = reify {
      prom.splice.complete(value.splice)
      c.Expr[Unit](Literal(Constant(()))).splice
    }

    def tryyIsFailure[A](tryy: Expr[Tryy[A]]): Expr[Boolean] = reify {
      tryy.splice.isFailure
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = reify {
      tryy.splice.get
    }
    def tryySuccess[A: WeakTypeTag](a: Expr[A]): Expr[Tryy[A]] = reify {
      scala.util.Success[A](a.splice)
    }
    def tryyFailure[A: WeakTypeTag](a: Expr[Throwable]): Expr[Tryy[A]] = reify {
      scala.util.Failure[A](a.splice)
    }
  }
}
