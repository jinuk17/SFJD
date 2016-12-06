package pub.jayden.scala.fis


import pub.jayden.scala.fis.chapter6.State
import pub.jayden.scala.fis.chapter7.Nonblocking.Par
import pub.jayden.scala.fis.chapter8.Gen
import pub.jayden.scala.fis.chapter9.Parsers

package object chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fa) => map(fa)(Right(_))
      }
  }

  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  def map2[A,B,C](fa: Gen[A], fb: Gen[B])(f: (A,B) => C): Gen[C] =
    fa flatMap (a => fb map (b => f(a,b)))

  def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fa flatMap (a => fb map (b => f(a,b)))


  trait Monad[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a,b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] = ???
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = ???
  }


  object Monad {
    val genMonad = new Monad[Gen] {
      def unit[A](a: => A): Gen[A] = Gen.unit(a)
      def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
        ma flatMap f
    }

    var parMonad = new Monad[Par] {
      def unit[A](a: => A): Par[A] = Par.unit(a)
      def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(fa)(f)
    }

    /*
    * Parsers 가 trait 인데 ...
    * */

    def parserMonad[Parser[_]](p: Parsers[Parser]) = new Monad[Parser] {
      def unit[A](a: => A): Parser[A] = p.succeed(a)
      def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] = p.flatMap(fa)(f)
    }

    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)
    }

    val streamMonad = new Monad[Stream] {
      def unit[A](a: => A): Stream[A] = Stream(a)
      def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa.flatMap(f)
    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)
      def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)
    }


    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f]{
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] =
        fa flatMap f
    }

    class StateMonad[S]{

      type AState[A] = State[S, A]

      def stateMonad = new Monad[AState] {
        def unit[A](a: => A): State[S, A] = State(s => (a, s))
        def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] =
          fa flatMap f
      }

    }




  }

}
