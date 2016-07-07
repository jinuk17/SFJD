
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
    def map[B](f: A => B): Stream[B] = ???

    //EXERCISE 7 append
    def append[B >: A](f: => B): Stream[B] = ???

    //EXERCISE 7 filter
    def filter(f: A => Boolean): Stream[A] = ???

    //EXERCISE 7 flatMap
    def flatMap[B](f: A => Stream[B]): Stream[B] = ???

    /*
    * EXERCISE 12: We can write a more general stream building function.
    * It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    * It is usually called unfold:
    * */

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???


    /*
    * EXERCISE 13: Use unfold to implement map, take, takeWhile, zip (as in chapter 3), and zipAll.
    * The zipAll function should continue the traversal as long as either stream has more elements —
    * it uses Option to indicate whether each stream has been exhausted.
    * */
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???



    /*
    * EXERCISE 14 (hard): implement startsWith using functions you've written.
    * It should check if one Stream is a prefix of another.
    * For instance, Stream(1,2,3) starsWith Stream(1,2) would be true.
    * */

    def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = ???


    /*
    * EXERCISE 15: implement tails using unfold.
    * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
    * So, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty).
    * */

    def tails: Stream[Stream[A]] = ???


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
  stream.toList
  stream.take(3).toList
  stream.takeWhile(_ <= 5).toList
  stream.takeWhileUsingFoldRight(_ <= 5).toList


  stream.headOptionUsingFoldRight
  stream.take(0).headOptionUsingFoldRight




  var ones: Stream[Int] = Stream.cons(1, ones)


  ones.take(5).toList
  ones.exists(_ % 2 != 0)


  /*
  * EXERCISE 8: Generalize ones slightly to the function constant which returns an infinite Stream of a given value.
  * */

  def constant[A](a: A): Stream[A] = ???


  /*
  * EXERCISE 9: Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, etc.
  * */

  def from(n: Int): Stream[Int] = ???

  /*
  * EXERCISE 10: Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  * */

  def fibs(n: Int): Stream[Int] = ???

  /*
  * EXERCISE 11: Write fibs, from, constant, and ones in terms of unfold.
  * */




}