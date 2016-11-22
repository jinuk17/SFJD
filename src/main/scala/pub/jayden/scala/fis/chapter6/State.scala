package pub.jayden.scala.fis.chapter6

import scala.annotation.tailrec

trait RNG{

  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
    (if(n1 > 0) n1 else -(n1 + 1), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng2) = nonNegativeInt(rng)
    (n1.toDouble / (Int.MaxValue.toDouble + 1d), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    ((n1, d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (n1, rng3) = rng2.nextInt
    ((d1, n1), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count > 0) {
      val (n1, rng2) = rng.nextInt
      val (list, rng3) = ints(count - 1)(rng2)
      (n1 :: list, rng3)
    } else (List(), rng)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(i: Int, r: RNG, l: List[Int]):(List[Int], RNG) = {
      if(i == 0)(l, r)
      else {
        val (n1, r2) = r.nextInt
        loop(i - 1, r2, n1::l)
      }
    }

    loop(count, rng, List())
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleUsingMap:Rand[Double] =
    map(nonNegativeInt)(n => n.toDouble / (Int.MaxValue.toDouble + 1d))


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def sequence[A](fs: List[Rand[A]]):Rand[List[A]] = {
    @tailrec
    def loop(rl: List[Rand[A]], l: List[A], r: RNG):(List[A], RNG) = {
      rl match {
        case x::xs => {
          val (n1, rng2) = x(r)
          loop(xs, n1 :: l, rng2)
        }
        case Nil => (l, r)
      }
    }
    rng => loop(fs, List(), rng)
  }

  def sequence1[A](fs: List[Rand[A]]):Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x, r) => map2(x, r)(_ :: _))


  def intsUsingSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if(i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (n, rng2) = f(rng)
      g(n)(rng2)
    }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

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
    fs.foldRight(unit[S, List[A]](List[A]()))((x, r) => x.map2(r)(_ :: _))
}

