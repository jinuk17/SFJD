package pub.jayden.scala.fis

import fpinscala.parallelism.Nonblocking.Par
import org.scalacheck.{Gen, Prop}

package object chapter10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }


  /*
  * Exercise 10.1
  * */

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = true
  }

  /*
  * Exercise 10.2
  * 순서는??.. 우선권이 a1에 있는데 ...
  * */

  def optionMonoidLeft[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    def zero = None
  }

//  def optionMonoidRight[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
//    def op(a1: Option[A], a2: Option[A]) = a2.orElse(a1)
//    def zero = None
//  }
  def optionMonoidRight[A]: Monoid[Option[A]] = inversParamOrder(optionMonoidLeft)

  /*
  * Exercise 10.3
  * 순서가 중요할까?
  * 순서가 중요하면 어떤걸 먼저 하는게 맞을까?
  * */
  def endoMonoidRight[A]: Monoid[ A => A] = new Monoid[A => A]{
    override def op(a1: (A) => A, a2: (A) => A) = a2 andThen a1
    override def zero = a => a
  }

//  def endoMonoidLeft[A]: Monoid[ A => A] = new Monoid[A => A]{
//    override def op(a1: (A) => A, a2: (A) => A) = a1 andThen a2
//    override def zero = a => a
//  }

  def endoMonoidLeft[A]: Monoid[ A => A] = inversParamOrder(endoMonoidRight)

  def inversParamOrder[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  /*
  * Exercise 10.4
  * */

  def monoidLaw[A](m: Monoid[A], gen:Gen[A]): Prop = {
    import org.scalacheck.Prop._

    def isAssociative: Prop = forAll(
      gen.flatMap(x => gen.flatMap(y => gen.map(z => (x, y, z))))
    )( a => m.op(a._1, m.op(a._2, a._3)) == m.op( m.op(a._1, a._2), a._3))

    def isAssociative_for: Prop = forAll(
      for{
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z)
    )( a => m.op(a._1, m.op(a._2, a._3)) == m.op( m.op(a._1, a._2), a._3))


    def isIdentity = forAll(gen)( a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

    isAssociative && isIdentity
  }

  val words = List("Hic", "Est", "Index")
  val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
  val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /*
  * Exercise 10.5
  * */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)( (a, b) => m.op(a, f(b)))
//    as.map(f).foldLeft(m.zero)(m.op)

  /*
  * Exercise 10.6
  * foldLeft, foldRight => foldMap
  * foldMap => foldLeft, foldRight
  *
  * monoid 는 같은 타입의 이항 연산을 지원 => foldLeft, foldRight 의 f (A, B) => B or (B, A) => B 함수를 처리할 방법이 없음.
  * foldMap 은 A타입의 List, B타입의 monoid 인스턴스와 A => B 함수f 를 입력값으로 받고 B 타입의 출력값을 준다.
  * 자기 함수를 통해 A => B를 A => (C => C) 으로 변경하고,
  * 함수 f를 currying으로 풀어서 A => C => C 로 시그니쳐를 맞춰준다.
  * foldMap 으로 A => ( B => B ) 형식으로 만들고 f1: B => B 으로 f1(z) 으로 결과 리턴.
  *
  *
  * List(1, 2, 3, 4, 5)
  *
  * f0: b => b
  * f1: b => f(b, 1)
  * f2: b => f(b, 2)
  * f3: b => f(b, 3)
  * f4: b => f(b, 4)
  * f5: b => f(b, 5)
  *
  * B => B = f0 andThen f5 andThen f4 andThen f3 andThen ....
  *
  *
  * words.foldLeft("")(_ + _) = (("" + "AAA") + "BBB") + "CCC" ...
  * 왼쪽부터 계산...
  * words.foldRight("")(_ + _) = ... "AAA" + ("BBB" + ("CCC" + ""))
  * 오른쪽부터 계산...
  * */

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val f1: A => B => B = a => b => f(b, a)
    foldMap(as, endoMonoidLeft[B])(f1)(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val f1: A => B => B = a => b => f(a, b)
    foldMap(as, endoMonoidRight[B])(f1)(z)
  }

  /*
  * Exercise 10.7
  *
  * TODO : refactoring
  * */
  
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val length = v.length
    if(length == 0) m.zero
    else if(length == 1) f(v.head)
    else {
      val c = length / 2
      val v1: B = foldMapV(v.slice(0, c), m)(f)
      val v2: B = foldMapV(v.slice(c, v.length), m)(f)
      m.op(v1, v2)
    }
  }

  /*
  * Exercise 10.8
  * */

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]{
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parMap: Par[IndexedSeq[B]] = Par.parMap(v)(f)
    Par.flatMap(parMap)(
      (bs:IndexedSeq[B]) => foldMapV(bs, par(m))( (b: B) => Par.async( _ => b)))
  }


  /*
  * Exercise 10.9
  * foldMap을 이용해 IndexedSeq[Int]가 정렬되어 있는지 점검..
  * */

  def isOrdered(intList: IndexedSeq[Int]): Boolean = foldMapV(intList, intOrderMonoid)(i => Some((i, true))).map(_._2).getOrElse(true)

  /*
  * 오름차순 ...
  * */
  def intOrderMonoid = new Monoid[Option[(Int, Boolean)]] {
    override def op(a1: Option[(Int, Boolean)], a2: Option[(Int, Boolean)]): Option[(Int, Boolean)] =
      (a1, a2) match {
        case (Some((i1, bool1)), Some((i2, bool2))) => {
          val b = if (i1 > i2) false else true
          Some((i2, bool1 && bool2 && b))
        }
        case (a, None) => a
        case (None, b) => b
      }

    override def zero: Option[(Int, Boolean)] = None
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC


  /*
  * Exercise 10.10
  *
  * 문자열을 한 문자씩 읽으면서 ... whitespace를 이용해 단어를 count
  * String == IndexedSeq[Char]
  * 결국 foldMapV를 이용한 병렬 처리를 위한 monoid가 필요..
  *
  * case ->
  *   Stub, Stub
  *   Stub, Part
  *   Part, Stub
  *   Part, Part
  * */

  val wcMonoid: Monoid[WC] = new Monoid[WC]{
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(ls2, wc2, rs2)) => Part(c1+ls2, wc2, rs2)
      case (Part(ls1, wc1, rs1), Stub(c2)) => Part(ls1, wc1, rs1+c2)
      case (Part(ls1, wc1, rs1), Part(ls2, wc2, rs2)) =>
        Part(ls1, wc1 + wc2 + (if ((rs1+ls2).isEmpty) 0 else 1) , rs2)
    }
    override def zero: WC = Stub("")
  }

  /*
  * Exercise 10.11
  * WC 모노이드를 이용해서 String 단어 개수를 세는 함수를 구현하라.
  * */

  def countWord(s: String): Int = {

    def countToNonEmptyString(s: String) =  if(s.isEmpty) 0 else 1

    val wc1: WC = foldMapV(s.toIndexedSeq, wcMonoid)(
      c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    )

    wc1 match {
      case Stub(s) => countToNonEmptyString(s)
      case Part(l, w, r) => countToNonEmptyString(l) + w + countToNonEmptyString(r)
    }
  }


  trait Foldable[F[_]]{
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)


    /*
    * Exercise 10.15
    * */
//    def toList[A](fa: F[A]): List[A] = ???
  }


  /*
  * Exercise 10.12
  * */

  def foldableList = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case ::(x, xs) => foldRight(xs)(f(x, z))(f)
      }
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case ::(x, xs) => foldLeft(xs)(f(z, x))(f)
      }

    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B ={
      as match {
        case Nil => mb.zero
        case ::(x, xs) => mb.op(f(x), foldMap(xs)(f)(mb))
      }

    }


//    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
//      as.foldLeft(mb.zero)( (a, b) => mb.op(a, f(b)))
  }


//  def foldableIndexedSeq = new Foldable[IndexedSeq] {
//    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = ???
//    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = ???
//    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = ???
//  }

//  def foldableStream = new Foldable[Stream] {
//    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = ???
//    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = ???
//    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = ???
//  }

  /*
  * Exercise 10.13
  * */

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def foldableTree = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(v) => f(v, z)
        case Branch(lt, rt) => {
          val rightValue = foldRight(rt)(z)(f)
          foldRight(lt)(rightValue)(f)
        }
      }
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(v) => f(z, v)
        case Branch(lt, rt) => {
          val leftValue = foldLeft(lt)(z)(f)
          foldLeft(rt)(leftValue)(f)
        }
      }
    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(v) => f(v)
        case Branch(lt, rt) => mb.op(foldMap(lt)(f)(mb), foldMap(rt)(f)(mb))
      }
  }

  /*
  * Exercise 10.14
  * */

//  def foldableOption = new Foldable[Option] {
//    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???
//    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = ???
//    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = ???
//  }


  /*
  * Exercise 10.16
  * */

//  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = ???



  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]]{
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) = {
        (a.keySet ++ b.keySet).foldLeft(zero) {
          (acc, k) => acc.updated(k, V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
      }

    }

  /*
  * Exercise 10.17
  * */

//  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = ???

  /*
  * Exercise 10.18
  * */
//  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???
}
