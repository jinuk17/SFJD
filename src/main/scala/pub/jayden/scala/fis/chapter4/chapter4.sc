


object OptionMain {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def flatMap1[B](f: A => Option[B]): Option[B] = {
      val a = getOrElse(None)
      if(a == None) None else f(a.asInstanceOf[A])
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map((x:A) => Some(x)).getOrElse(ob)
    }

    def orElse1[B >: A](ob: => Option[B]): Option[B] = {
      if(getOrElse(None) == None) ob else this
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap((x:A) => if(f(x)) Some(x) else None)
    }

    def filter1(f: A => Boolean): Option[A] = {
      val a = getOrElse(None)
      if(a == None|| f(a.asInstanceOf[A])) None else this
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  //Exercise 2
  def variance(xs: Seq[Double]): Option[Double] = {

    def average(ys: Seq[Double]): Option[Double] = {
      if (ys.isEmpty) None
      else Some(ys.sum / ys.size)
    }

    def calculate(xs: Seq[Double])(m: Double): Option[Double] = {
      if(xs.isEmpty) None
      else Some(xs.foldLeft(0.0)((a, b) => a + math.pow(b + m, 2)) / xs.size)
    }

    //average(xs).flatMap(calculate(xs)_)

    average(xs).flatMap(x => average(xs.map( y => math.pow(y + x, 2))))
  }

  variance(Seq(1,2,3,4,5,6,7,7))


  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat).map(p => (s: String) => p.matcher(s).matches )
  }

  def doesMatch(pat: String, s: String): Option[Boolean] =
    mkMatcher(pat) map (x => x(s))

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch_1(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)


  /*
  * EXERCISE 3: bothMatch is an instance of a more general pattern.
  * Write a generic function map2, that combines two Option values using a binary function.
  * If either Option value is None, then the return value is too. Here is its signature:
  * */

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (x => b map (y => f(x, y)))
  }

  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x,y)

  val a = Some("jayden")
  val b = Some("uk")


  map2(a, b)(_ + "." + _)
  map2_1(a, b)(_ + "." + _)

  /*
  * EXERCISE 4: Re-implement bothMatch above in terms of this new function, to the extent possible.
  * */

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))
  }


  /*
  * EXERCISE 5: Write a function sequence, that combines a list of Options into one option containing a list of all the Some values in the original list.
   * If the original list contains None even once, the result of the function should be None, otherwise the result should be Some with a list of all the values.
   * Here is its signature:
  * */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(List[A]()):Option[List[A]])((b, a) => a.flatMap(x => b map (y => y :: x)))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(List[A]()):Option[List[A]])((b, a) => map2(a, b)((x, y) => y :: x))
  }

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(
      Some(List[A]()):Option[List[A]]
    )(
      (a, b) => map2(a, b)((x, y) => y :: x)
    ) map (l => l.reverse)
  }

  val list = List(
    Some(1), Some(7), Some(6), Some(5), Some(4), Some(3), Some(2)
  )

  val listHasNone = List(
    Some(1), None, Some(7), Some(6), Some(5), Some(4), Some(3), Some(2)
  )

  sequence(list)
  sequence(listHasNone)

  sequence_1(list)
  sequence_1(listHasNone)

  /*
  * EXERCISE 6: Implement this function.
  * It is straightforward to do using map and sequence, but try for a more efficient implementation that only looks at the list once.
  * In fact, implement sequence in terms of traverse.
  * */

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    a.foldLeft(
      Some(List[B]()):Option[List[B]]
    )(
      (a, b) => a.flatMap(x => f(b) map (y => y :: x))
    ) map (l => l.reverse)
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

  sequenceUsingTraverse(list)
  sequenceUsingTraverse(listHasNone)

  def traverseUsingSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a map (x => f(x)))
  }


  /*
  * EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
  * */

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
      case Left(e) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]  =
      flatMap(x => b map (y => f(x, y)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]





  /*
  * EXERCISE 8: Implement sequence and traverse for Either.
  * */



  /*
  * EXERCISE 9: In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
  * What would you need to change in order to report both errors?
  * Would you change map2 or the signature of mkPerson?
  * Or could you create a new data type that captures this requirement better than Either does, with some additional structure?
  * How would orElse, traverse, and sequence behave differently for that data type?
  * */


}
