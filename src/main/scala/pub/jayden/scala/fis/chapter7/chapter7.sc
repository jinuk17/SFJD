import scala.concurrent.duration.TimeUnit

object chapter7{

  class ExecutorService{
    def submit[A](a: Callable[A]): Future[A] = ???
  }

  trait Callable[A] {
    def call: A
  }

  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(eventIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }


  type Par[A] = ExecutorService => Future[A]


  object Par{

    //def map2[A, B](a: A, b: A)(f: (A, A) => B) = ???

    def unit[A](a: A) : Par[A] = es => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {

      override def isDone: Boolean = true
      override def get(timeout: Long, unit: TimeUnit): A = get
      override def isCancelled: Boolean = false
      override def cancel(eventIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))


    def lazyUnit[A](a: A): Par[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = ???


    def asyncF[A, B](f: A => B): A => Par[B] =
      a => unit(f(a))

    def sortPar1(parList: Par[List[Int]]): Par[List[Int]] =
      map2(parList, unit(()))((a, _) => a.sorted)

    def sortPar2(parList: Par[List[Int]]) = map(parList)(_.sorted)


    def parMap[A, B](ps: List[A])(f: A=>B): Par[List[B]] = fork {
      val fbs = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ???

  }



}