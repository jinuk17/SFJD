object EitherMain {
  /*
 * EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
 * */

  val list = List(
    Right(1), Right(7), Right(6), Right(5), Right(4), Right(3), Right(2)
  )

  val listHasLeft = List(
    Right(1), Left("Error"), Right(7), Right(6), Right(5), Right(4), Left("Error"), Right(2)
  )
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[B >: A](b: => Either[E, B]): Either[E, B] =
      this match{
        case Left(_) => b
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

  def sequence[EE >: E, E, A](a: List[Either[E, A]]): Either[EE, List[A]] =
    a.foldLeft(Right(List[A]()):Either[EE, List[A]])((a, b) => a.flatMap(x => b.map(y =>  y :: x))).map(x => x.reverse)

  def sequence_1[EE >: E, E, A](a: List[Either[E, A]]): Either[EE, List[A]] =
    a.foldLeft(Right(List[A]()):Either[EE, List[A]])((a, b) =>
      for{
        x <- a
        y <- b
      } yield (y :: x)
    ) map (_.reverse)

  sequence(list)
  sequence(listHasLeft)

  sequence_1(list)
  sequence_1(listHasLeft)



  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldLeft(Right(List[B]()):Either[E, List[B]])((a, b) => a.flatMap(x => f(b) map (y => y :: x))).map(_.reverse)


  def traverseFromSequence[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(a.map(f(_)))

  def sequenceFromTraverse[EE >: E, E, A](a: List[Either[E, A]]): Either[EE, List[A]] =
    traverse(a)(x => x)


  val intList = List(1, 2, 3, 4, 5, 6)

  traverse(intList)(Right(_))

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)


  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))


  /*
  * EXERCISE 9: In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
  * What would you need to change in order to report both errors?
  * Would you change map2 or the signature of mkPerson?
  * Or could you create a new data type that captures this requirement better than Either does, with some additional structure?
  * How would orElse, traverse, and sequence behave differently for that data type?
  * */



}