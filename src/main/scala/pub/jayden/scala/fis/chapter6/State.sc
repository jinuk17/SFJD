import scala.annotation.tailrec

object Chapter6_1{

  trait RNG{
    def nextInt:(Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /*
  * 연습문제 6.10
  * */

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        g(a).run(s1)
      })
  }

  object State {

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      @tailrec
      def loop(rl: List[State[S, A]], l: List[A], s: S): (List[A], S) = {
        rl match {
          case x :: xs => {
            val (a, s2) = x.run(s)
            loop(xs, a :: l, s2)
          }
          case Nil => (l, s)
        }
      }
      State(s => loop(fs, List(), s))
    }

    def sequence1[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight((unit[S, List[A]](List[A]())))((x, r) => x.map2(r)(_ :: _))
  }

  type RAND[A] = State[RNG, A]

}