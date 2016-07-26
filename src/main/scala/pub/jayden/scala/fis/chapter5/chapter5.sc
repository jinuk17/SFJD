object StreamMain{

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    /*
    * EXERCISE 1: Write a function to convert a Stream to a List,
    * which will force its evaluation and let us look at it in the REPL.
    * You can convert to the regular List type in the standard library.
    * You can place this and other functions that accept a Stream inside the Stream trait.
    * */
    def toList: List[A] = this match {
      case Empty => List()
      case Cons(h, t) => h() :: (t() toList)
    }

    /*
    *EXERCISE 2: Write a function take for returning the first n elements of a Stream.
    * */
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if(n > 0) => Cons(h,  () => t().take(n-1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if(n > 0) => t().drop(n-1)
      case _ => Empty
    }

    /*
    *EXERCISE 3: Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
    * */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if(p(h())) => Cons(h,  () => t().takeWhile(p))
      case _ => Empty
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    /*
    * EXERCISE 4: Implement forAll, which checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a non-matching value.
    * */
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    /*
    * EXERCISE 5: Use foldRight to implement takeWhile.
    * This will construct a stream incrementally, and only if the values in the result are demanded by some other expression.
    * */
    def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Empty:Stream[A])((a, b) => if(p(a)) Cons(() => a, () => b) else Empty)

    //EXERCISE 6 Implement headOption using foldRight.
    def headOptionUsingFoldRight: Option[A] =
      foldRight(None:Option[A])((a, b) => Some(a))

    /*
    * EXERCISE 7: Implement map, filter, append, and flatMap using foldRight.
    * */
    //EXERCISE 7 map
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream():Stream[B])((a, b) => Cons(() => f(a),() => b))

    //EXERCISE 7 append
    def append[B >: A](s: Stream[B]): Stream[B] =
      foldRight(s)((a, b) => Cons(() => a, () => b))

    //EXERCISE 7 filter
    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream():Stream[A])((a, b) => if(f(a)) Cons(() => a, () => b) else b)

    //EXERCISE 7 flatMap
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream():Stream[B])((a, b) => f(a) append b)

    /*
    * EXERCISE 13: Use unfold to implement map, take, takeWhile, zip (as in chapter 3), and zipAll.
    * The zipAll function should continue the traversal as long as either stream has more elements —
    * it uses Option to indicate whether each stream has been exhausted.
    * */

    def mapUsingUnfold[B](f: A => B): Stream[B] =
      unfold(this)({
          case Cons(h, t) => Some(f(h()), t())
          case _ => None
      })

    def takeUsingUnfold(n: Int): Stream[A] = unfold(this, n)({
        case (Cons(h, t), i) if(i > 0)  => Some(h(), (t(), i-1))
        case _ => None
    })
    def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = unfold(this)({
      case Cons(h, t) if(p(h())) =>  Some(h(), t())
      case _ => None
    })
    def zipWith[B, C](s2:Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, s2)({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    })

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Stream())))
      case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream(), t2())))
      case _ => None
    }


    /*
      * EXERCISE 14 (hard): implement startsWith using functions you've written.
      * It should check if one Stream is a prefix of another.
      * For instance, Stream(1,2,3) starsWith Stream(1,2) would be true.
      * */

    def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) => if(h1() == h2()) t1() startsWith t2() else false
      case _ => true
    }

    def startsWith1[A](s: Stream[A]): Boolean =
      (this zipAll s) takeWhile (x => x._2 != None ) forAll (x => x._1 == x._2)
    /*
    * EXERCISE 15: implement tails using unfold.
    * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
    * So, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty).
    * */

    def tails: Stream[Stream[A]] = unfold(this)({
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    }) append Stream(Empty)


    def tails1: Stream[Stream[A]] =
      unfold(this)( x => if(x == Empty ) None else Some((x , x.drop(1))))

    /*
    * EXERCISE 16 (hard, optional): Generalize tails to the function scanRight,
    * which is like a foldRight that returns a stream of the intermediate results. For example:
    * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    * res0: List[Int] = List(6,5,3,0)
    * This example would be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
    * Your function should reuse intermediate results so that traversing a Stream with n elements always takes time linear in n.
    * Can it be implemented using unfold? How, or why not?
    * Could it be implemented using another function we have written?
    * */


    /*
    * N 에 선형비례하지 않는다.
    * N 에 선형비례 -> 중간 결과를 재사용 -> foldRight 사용
    * unfold는 head에서 부터 접근해서 사용해서 N에 선형비례하지 않음 ...
    * */
    def scanRight[B](a: B)(f: (A,=> B) => B): Stream[B] = {
      unfold(this)({
        case Cons(h, t) => Some(Cons(h, t).foldRight(a)(f), t())
        case _ => None
      }) append Stream(a)
    }


    def scanRight1[B](a: B)(f: (A, => B) => B): Stream[B] =
      foldRight((a, Stream(a)))((e, acc) => {
        val v: B = f(e, acc._1)
        (v, Stream.cons(v, acc._2))
      })._2
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


  object Stream{
    def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as:A*): Stream[A] =
      if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

  val stream = Stream(1, 2, 3, 4, 5, 6, 7)
  val stream2 = Stream(100, 200, 300, 400)


  val toListResult = stream.toList

  val takeResult = stream.take(3).toList
  val takeUsingUnfoldResult = stream.takeUsingUnfold(5).toList

  val takeWhileResult = stream.takeWhile(_ <= 5).toList
  val takeWhileUsingFoldRightResult = stream.takeWhileUsingFoldRight(_ <= 5).toList
  val takeWhileUsingUnfoldResult = stream.takeWhileUsingUnfold(_ <= 5).toList

  val headOptionUsingFoldRightResult1 = stream.headOptionUsingFoldRight
  val headOptionUsingFoldRightResult2 = stream.take(0).headOptionUsingFoldRight

  val mapResult = stream.map(_.toString() + " jayden").toList
  val mapUsingUnfoldResult = stream.mapUsingUnfold(_.toString() + " uk").toList


  val appendResult = stream append stream2 toList

  val filterResult = stream.filter(_ > 3).toList

  val flatMapResult = stream.flatMap(x => Stream(x, x, x)).toList


  val ones: Stream[Int] = Stream.cons(1, ones)


  ones.take(5).toList
  ones.exists(_ % 2 != 0)

  val constantStream  = constant(2)

  constantStream.take(5).toList

  val fromStream = from(1)

  fromStream.take(10).toList

  val fibsStream = fibs

  fibsStream.take(10).toList



  val stream3 = Stream(1, 2, 3)
  val stream4 = Stream(5, 6, 7)

  val startsWithTrueResult = stream startsWith stream3
  val startsWithFalseResult = stream startsWith stream4
  val startsWith1TrueResult = stream startsWith1 stream3
  val startsWith1FalseResult = stream startsWith1 stream4

  var result = stream3.scanRight1(0)(_+_)
  val scanRightResult = result.toList



  val tailsResult = ((stream3 tails) toList) map (_.toList)

  Stream(1).take(1).toList


  /*
  * EXERCISE 8: Generalize ones slightly to the function constant which returns an infinite Stream of a given value.
  * */

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def constant1[A](a: A): Stream[A] = {
    lazy val cons: Stream[A] = Cons(() => a, () => cons)
    cons
  }

  constant1(10).take(5).toList

  /*
  * EXERCISE 9: Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, etc.
  * */

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  /*
  * EXERCISE 10: Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  * */

  def fibs: Stream[Int] = {
    def loop(a: Int,b: Int): Stream[Int] = {
      Stream.cons(a, loop(b, a+b))
    }
    loop(0, 1)
  }
  def fib(): Stream[Int] = {

    def loop(a1:Int, a2:Int): Stream[Int] = {
      Stream.cons[Int](a1 + a2, loop(a2, a1 + a2))
    }
    Stream.cons(0, Stream.cons(1, loop(0, 1)))
  }

  fibs.take(10).toList
  fib.take(10).toList

  /*
  * EXERCISE 11: We can write a more general stream building function.
  * It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  * It is usually called unfold:
  * */

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some(x) => Stream.cons(x._1, unfold(x._2)(f))
      case None => Empty
    }

  /*
  * EXERCISE 12: Write fibs, from, constant, and ones in terms of unfold.
  * */

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)((x:A) => Option((x, x)))
  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(x => Option(x, x+1))
  def fibsUsingUnfold:Stream[Int] = {
    def tupleNextSum(a: (Int, Int)):(Int, Int) = (a._2, a._1 + a._2)
    unfold((0,1))(x => Option(x._1, tupleNextSum(x)))
  }


  val onesUsingUnfold = unfold(1)(x => Option((1, 1)))
  onesUsingUnfold.take(5).toList

  val constantUsingUnfoldStream  = constantUsingUnfold(2)
  constantUsingUnfoldStream.take(5).toList

  val fromUsingUnfoldStream = fromUsingUnfold(100)
  fromUsingUnfoldStream.take(5).toList

  val fibsUsingUnfoldStream = fibsUsingUnfold
  fibsUsingUnfoldStream.take(10).toList


}