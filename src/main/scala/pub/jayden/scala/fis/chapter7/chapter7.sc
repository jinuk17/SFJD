import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

import pub.jayden.scala.fis.chapter7.Actor

import scala.language.implicitConversions

object chapter7{

//  class ExecutorService{
//    def submit[A](a: Callable[A]): Future[A] = ???
//  }

//  trait Callable[A] {
//    def call: A
//  }

//  trait Future[A] {
//    def get: A
//    def get(timeout: Long, unit: TimeUnit): A
//    def cancel(eventIfRunning: Boolean): Boolean
//    def isDone: Boolean
//    def isCancelled: Boolean
//  }

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }


  type Par[A] = ExecutorService => Future[A]


  object Par{

    //def map2[A, B](a: A, b: A)(f: (A, A) => B) = ???

//    def unit[A](a: A) : Par[A] = es => UnitFuture(a)

    def unit[A](a: A) : Par[A] =
      es => new Future[A] {
        def apply(k: (A) => Unit): Unit = k(a)
      }

//    private case class UnitFuture[A](get: A) extends Future[A] {
//
//      override def isDone: Boolean = true
//      override def get(timeout: Long, unit: TimeUnit): A = get
//      override def isCancelled: Boolean = false
//      override def cancel(eventIfRunning: Boolean): Boolean = false
//    }

//    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
//      es => {
//        val af = a(es)
//        val bf = b(es)
//        UnitFuture(f(af.get, bf.get))
//      }
      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        es => new Future[C]{
          def apply(k: (C) => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None

            val combiner = Actor[Either[A, B]](es){
              case Left(a) => br match {
                case None => ar = Some(a)
                case Some(b) => eval(es)(k(f(a, b)))
              }

              case Right(b) => ar match {
                case None => br = Some(b)
                case Some(a) => eval(es)(k(f(a, b)))
              }
            }

            a(es)(a => combiner ! Left(a))
            b(es)(b => combiner ! Right(b))
          }
        }

//    def fork[A](a: => Par[A]): Par[A] =
//      es => es.submit(new Callable[A] {
//        def call = a(es).get
//      })

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(k: (A) => Unit): Unit = eval(es)(a(es)(k))
      }

    def eval(es: ExecutorService)(r: => Unit):Unit =
      es.submit(new Callable[Unit] { def call = r})

    def delay[A](a: => Par[A]): Par[A] =
      es => a(es)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))


    def lazyUnit[A](a: A): Par[A] = delay(unit(a))



//    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def run[A](s: ExecutorService)(a: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)

      a(s){ a => {
          ref.set(a)
          latch.countDown
        }
      }

      latch.await
      ref.get
    }


    def asyncF[A, B](f: A => B): A => Par[B] =
      a => unit(f(a))

    def sortPar1(parList: Par[List[Int]]): Par[List[Int]] =
      map2(parList, unit(()))((a, _) => a.sorted)

    def sortPar2(parList: Par[List[Int]]) = map(parList)(_.sorted)


    def parMap[A, B](ps: List[A])(f: A=>B): Par[List[B]] = delay {
      val fbs = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      //완전 순차처리...      unit(as.filter(f(_)))
      map(sequence(as.map(asyncF(a => if (f(a)) List(a) else List()))))(_.flatten)
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ ::_ ))


    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        if(run(es)(cond)) t(es) else f(es)


    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es =>
        choices(run(es)(n))(es)

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      es =>
        choices(run(es)(key))(es)

    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es =>
        choices(run(es)(pa))(es)


    def choiceUsingChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(cond)(if(_) t else f)

    def choiceNUsingChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(n)(choices(_))

    def choiceMapUsingChooser[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      chooser(key)(choices(_))


    def join[A](pa: Par[Par[A]]): Par[A] = ???

  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f:A => B): Par[B] = Par.map(p)(f)
    def run(s: ExecutorService): A = Par.run(s)(p)
  }


  val S = Executors.newFixedThreadPool(4)

  val echoer = Actor[String](S){
    msg => println (s"Got message: '$msg'")
  }

  echoer ! "hello"

  echoer ! "goodbye"

  echoer ! "You're just repeating everything I say, aren't you?"

  val p: Par[List[Double]] = Par.parMap(List.range(1, 100000))(Math.sqrt(_))
  val x  = p.run(Executors.newFixedThreadPool(2))

  //val x = Par.run(Executors.newFixedThreadPool(2))(p)

}

object NonBlocking{

  type Par[A] = ExecutorService => Future[A]

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(eventIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  /* This version respects timeouts. See `Map2Future` below. */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
  }

  /*
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }
}