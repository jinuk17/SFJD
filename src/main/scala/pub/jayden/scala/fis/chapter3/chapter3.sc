
  sealed trait LocalList[+A]
  case object LocalNil extends LocalList[Nothing]
  case class LocalCons[+A](head: A, tail: LocalList[A]) extends LocalList[A]

  //EXERCISE 2 Answer

  object LocalList {
    def sum(ints: LocalList[Int]): Int = ints match {
      case LocalNil => 0
      case LocalCons(x, xs) => x + sum(xs)
    }

    def product(ds: LocalList[Double]): Double = ds match {
      case LocalNil => 1.0
      case LocalCons(0.0, _) => 0.0
      case LocalCons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): LocalList[A] =
      if(as.isEmpty) LocalNil
      else LocalCons(as.head, apply(as.tail: _*))
  }

  /*
   * EXERCISE 3.1
   * 다음 패턴 부합 표현식의 결과는 무엇인가?
   */

  val list:LocalList[Int] = LocalList(1, 2, 3, 4, 5)

  val x = list match {
    case LocalCons(x, LocalCons(2, LocalCons(4, _))) => x
    case LocalNil => 42
    case LocalCons(x, LocalCons(y, LocalCons(3, LocalCons(4, _)))) => x + y
    case LocalCons(h, t) => h + LocalList.sum(t)
    case _ => 101
  }

  def y:PartialFunction[LocalList[Int], Int] = {
    case LocalCons(x, LocalCons(2, LocalCons(4, _))) => x
    case LocalNil => 42
    case LocalCons(x, LocalCons(y, LocalCons(3, LocalCons(4, _)))) => x + y
    case LocalCons(h, t) => h + LocalList.sum(t)
    case _ => 101
  }

  /*
    * EXERCISE 2: Implement the function tail for "removing" the first element of a List.
    * Notice the function takes constant time.
    * What are different choices you could make in your implementation if the List is Nil?
    * We will return to this question in the next chapter.
    * */

  def tail[A](a:LocalList[A]): LocalList[A] = a match {
    case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
    case LocalCons(x, xs) => xs
  }

  val tailResult = tail(list)

  /*
  * EXERCISE 3: Generalize tail to the function drop, which removes the first n elements from a list.
  * */
  def drop[A](l: LocalList[A], n: Int): LocalList[A] =
    if(n == 0) l else drop(tail(l), n-1)

  val dropResult = drop(list, 3)

  /*
  *EXERCISE 4: Implement dropWhile,10 which removes elements from the List prefix as long as they match a predicate.
  * Again, notice these functions take time proportional only to the number of elements being dropped—we do not need to make a copy of the entire List.
  *  */
  def dropWhile[A](l: LocalList[A])(f: A => Boolean): LocalList[A] = l match {
    case LocalCons(x, xs) => if(f(x)) dropWhile(xs)(f) else LocalCons(x, xs)
  }

  val dropWhileResult = dropWhile(list)(_ < 3)
  /*
  * EXERCISE 5: Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  * */
  def setHead[A](l: LocalList[A])(h: A): LocalList[A] = l match {
    case LocalNil => LocalCons(h, LocalNil)
    case LocalCons(x, xs) => LocalCons(h, xs)
  }

  val setHeadResult = setHead(list)(10)

  /*
  * EXERCISE 6: Not everything works out so nicely.
  * Implement a function, init, which returns a List consisting of all but the last element of a List.
  * So, given List(1,2,3,4), init will return List(1,2,3).
  * Why can't this function be implemented in constant time like tail?
  * */

  def init[A](l: LocalList[A]): LocalList[A] = l match {
    case LocalNil =>  throw new IllegalArgumentException("LocalNil is not supported.")
    case LocalCons(x, LocalNil) => LocalNil
    case LocalCons(x, xs) => LocalCons(x, init(xs))
  }

  val initResult = init(list)