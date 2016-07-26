
import scala.annotation.tailrec

object Chapter6{

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

  val rng = SimpleRNG(42)

  val (n1, rng2) = rng.nextInt

  val (n2, rng3) = rng2.nextInt

  /*
  * 연습문제 6.1
  * 0 <= n <= Int.Maxvalue
  * */

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng2) = rng.nextInt
    (if(n1 > 0) n1 else -(n1 + 1), rng2)
  }

  /*
  * 연습문제 6.2
  * 0 <= n < 1
  * */

  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng2) = nonNegativeInt(rng)
    (n1.toDouble / (Int.MaxValue.toDouble + 1d), rng2)
  }

  val (d1, rng4) = double(rng)
  val (d2, rng5) = double(rng4)
  val (d3, rng6) = double(rng5)
  val (d4, rng7) = double(rng6)

  /*
  * 연습문제 6.3
  * */

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

  /*
  * 연습문제 6.4
  * */

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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  unit(1)
  unit("AA")


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /*
  * 연습문제 6.5
  * */

  def doubleUsingMap:Rand[Double] =
    map(nonNegativeInt)(n => n.toDouble / (Int.MaxValue.toDouble + 1d))



  val (d100, rng100) = double(rng)
  val (d101, rng101) = doubleUsingMap(rng)


  /*
  * 연습문제 6.6
  * */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


  /*
  * 연습문제 6.7
  *
  * foldRight 로 변경 할수 있을듯 ...
  * */

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
    fs.foldRight((unit(List[A]())))((x, r) => map2(x, r)(_ :: _))


  def intsUsingSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  intsUsingSequence(10)(rng)


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if(i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }


  /*
  * 연습문제 6.8
  * */

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (n, rng2) = f(rng)
      g(n)(rng2)
    }

  /*
  * 연습문제 6.9
  * */

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => ((f(a), rng)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  val zero = rollDie(SimpleRNG(5))._1


//  def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = ???


  /*
  * 연습문제 6.11
  * */


}
