package pub.jayden.scala.fis.chapter8

import pub.jayden.scala.fis.chapter6.{RNG, SimpleRNG, State}
import pub.jayden.scala.fis.chapter8.Prop._

//trait Prop {
//  def check: Boolean
//  def && ( p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
//}

case class Prop( run: (MaxSize, TestCases, RNG) => Result ){

  /*
  * Exercise 8.9
  */

  def &&(p: Prop): Prop = Prop{
    (max, n, rng) => run(max, n, rng) match{
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop{
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }

  }

  def tag(msg: String) = Prop {
    (max, n,rng) => run(max, n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {

  def apply(f: (TestCases,RNG) => Result): Prop = Prop { (_,n,rng) => f(n,rng) }


  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result{
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  type FailedCase = String
  type SuccessCount = Int

  type MaxSize = Int
  type TestCases = Int


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casePerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take( (n min max) + 1 ).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop {
          (max, _, rng) => p.run(max, casePerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def unfold[A, B](start: B)(f: B => Option[(A, B)]): Stream[A] = f(start) match {
    case Some((elem, next)) => Stream.cons(elem, unfold(next)(f))
    case None => Stream[Nothing]()
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))


  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsfied after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

}

case class Gen[+A](sample: State[RNG, A]) {

  /*
  * Exercise 8.6
  */

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap( Gen.listOfN(_, this))

  /*
  * Exercise 8.10
  */

  def unsized: SGen[A] = SGen{ n => this }

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

}

object Gen {

  /*
  * Exercise 8.4
  */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /*
  * Exercise 8.5
  */

  def unit[A](a: => A) : Gen[A] = Gen(State.unit(a))

  //S => (A, S)
  //State[RNG, A] RNG => (RNG, Boolean)
  def boolean: Gen[Boolean] = Gen(State(
    _.nextInt match {
      case (i, rng2) => (i%2==0, rng2)
    }
  ))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  /*
  * Exercise 8.7
  */

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)

  /*
  * Exercise 8.8
  */

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State(RNG.double).flatMap(
        d => if(d < g1Threshold) g1._1.sample else g2._1.sample
      )
    )
  }

  /*
  * Exercise 8.12
  */

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen{
    n => listOfN(n, g)
  }

  /*
  * Exercise 8.13
  * */

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen{
    n => Gen.listOfN(1, g)
  }

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  val uniform: Gen[Double] = Gen(State(RNG.double))

}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  /*
  * Exercise 8.11
  */

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(
    n => forSize(n).flatMap(f(_).forSize(n))
  )

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))


}
