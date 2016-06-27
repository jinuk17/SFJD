import scala.annotation.tailrec

sealed trait LocalList[+A]
case object LocalNil extends LocalList[Nothing]
case class LocalCons[+A](head: A, tail: LocalList[A]) extends LocalList[A]

object LocalList {
  def sum(ints: LocalList[Int]): Int = ints match {
    case LocalNil => 0
    case LocalCons(x, xs) => x + sum(xs)
  }

  def product(ds: LocalList[Double]): Double = ds match {
    case LocalNil => 1.0
    case LocalCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): LocalList[A] =
    if(as.isEmpty) LocalNil
    else LocalCons(as.head, apply(as.tail:_*))
}

/*
 * EXERCISE 3.1
 * 다음 패턴 부합 표현식의 결과는 무엇인가?
 *
 */

val list:LocalList[Int] = LocalList(1, 2, 3, 4, 5)
// LocalCons(1, LocalCons(2,  LocalCons(3,  LocalCons(4, LocalCons(5, LocalNil)))))

val x = list match {
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

def tail[A](l:LocalList[A]): LocalList[A] = l match {
  case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
  case LocalCons(x, xs) => xs
  //case y: LocalCons[A] => y.tail
}

val tailResult = tail(list)

/*
* EXERCISE 3: Generalize tail to the function drop, which removes the first n elements from a list.
* */
@tailrec
def drop[A](l: LocalList[A], n: Int): LocalList[A] =
  if(n == 0) l else drop(tail(l), n-1)

val dropResult = drop(list, 3)

/*
*EXERCISE 4: Implement dropWhile,10 which removes elements from the List prefix as long as they match a predicate.
* Again, notice these functions take time proportional only to the number of elements being dropped—we do not need to make a copy of the entire List.
*  */
@tailrec
def dropWhile[A](l: LocalList[A])(f: A => Boolean): LocalList[A] = l match {
  case LocalNil => l
  case LocalCons(x, xs) => if(f(x)) dropWhile(xs)(f) else l
}

val dropWhileResult = dropWhile(list)(_ < 3)

//def dropWhile1[A](l: LocalList[A])(f: A => Boolean): LocalList[A] = ???
//def dropWhile2[A](l: LocalList[A], f: A => Boolean): LocalList[A] = ???
//
//dropWhile1(list)(x => x>0)
//dropWhile2(list, x => x>0)
/*
* EXERCISE 5: Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
* */
def setHead[A](l: LocalList[A])(h: A): LocalList[A] = l match {
  case LocalNil => throw new IllegalArgumentException("LocalNil don't have head.")
  case LocalCons(x, xs) => LocalCons(h, xs)
}

//val setHeadResult = setHead(list) _
//setHeadResult(10)

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

def foldRight[A,B](l: LocalList[A], z: B)(f: (A, B) => B): B = l match {
    case LocalNil => z
    case LocalCons(x, xs) => f(x, foldRight(xs, z)(f))
}

/*
* EXERCISE 7: Can product implemented using foldRight immediately halt the recursion and return 0.0
*  if it encounters a0.0? Why or why not?
* Consider how any short-circuiting might work if you call foldRight with a large list.
* This is a deeper question that we'll return to a few chapters from now.
* */

object FoldRight{
  def product(ds: LocalList[Double]): Double = foldRight(ds, 1.0)(_ * _)
}

/*
* EXERCISE 8: See what happens when you pass Nil and Cons themselves to foldRight,
* like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
* What do you think this says about the relationship between foldRight and the data constructors of List?
* */

foldRight(LocalList(1,2,3), LocalNil:LocalList[Int])(LocalCons(_,_))

// answer : like, Cons(1, Cons(2, Cons(3, LocalNil)))


/*
* EXERCISE 9: Compute the length of a list using foldRight.
* */
def length[A](l: LocalList[A]): Int = foldRight(l, 0)((x,y) => y+1)

val lengthResult = length(tail(list))

/*
* EXERCISE 10: foldRight is not tail-recursive and will StackOverflow for large lists.
* Convince yourself that this is the case, then write another general list-recursion function, foldLeft that is tail-recursive, using the techniques we discussed in the previous chapter.
* */

  @tailrec
  def foldLeft[A,B](l: LocalList[A], z: B)(f: (B, A) => B): B = l match {
    case LocalNil => z
    case LocalCons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
  * EXERCISE 11: Write sum, product, and a function to compute the length of a list using foldLeft.
  * */

object FoldLeft{
  def sum(ints: LocalList[Int]): Int = foldLeft(ints, 0)(_+_)

  def product(ds: LocalList[Double]): Double = foldLeft(ds, 1.0)(_*_)

  def length[A](l: LocalList[A]): Int = foldLeft(l, 0)((x,y) => x+1)
}

  /*
  * EXERCISE 12: Write a function that returns the reverse of a list (so given List(1,2,3) it returns List(3,2,1)).
  * See if you can write it using a fold.
  * */

def reverse[A](l: LocalList[A]): LocalList[A] =
  foldLeft(l, LocalNil:LocalList[A])((x, y) => LocalCons(y,x))

reverse(list)

  /*
  * EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight? How about the other way around?
  * */


//list 를 reverse 한 다음 foldRight 호출
def foldLeftByFoldRight[A,B](l: LocalList[A], z: B)(f: (B, A) => B): B = {
  //with foldRight
  def append(xs: LocalList[A], x: A): LocalList[A] = {
    foldRight(xs, LocalCons(x, LocalNil))((y, ys) => LocalCons(y, ys))
  }
  val reverse: LocalList[A] = foldRight(l, LocalNil: LocalList[A])((a, b) => append(b, a))

  foldRight(reverse, z)((b, a) => f(a, b))
}

foldLeftByFoldRight(list, 0)(_+_)

//list 를 reverse 한 다음 foldLeft 호출
def foldRightByFoldLeft[A,B](l: LocalList[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b)=> f(b, a))

  /*
  * EXERCISE 14: Implement append in terms of either foldLeft or foldRight.
  * */
//with foldLeft
def append[A](as: LocalList[A], xs: LocalList[A]): LocalList[A] =
    foldRight(as, xs)(LocalCons(_, _))

/*
* EXERCISE 15 (hard): Write a function that concatenates a list of lists into a single list.
* Its runtime should be linear in the total length of all lists.
* Try to use functions we have already defined.
* */

def flatMapForLocalList[A](l: LocalList[LocalList[A]]):LocalList[A] = {
  foldRight(l, LocalNil:LocalList[A])(
    (list, xs) => foldRight(list, xs)(
      (x, ys) => LocalCons(x, ys)
    )
  )
}


val lists:LocalList[LocalList[Int]] = LocalList(list, list, list, list, list)
flatMapForLocalList(lists)


/*
* EXERCISE 16: Write a function that transforms a list of integers by adding 1 to each element.
* (Reminder: this should be a pure function that returns a new List!)
* */

def mapForPlusOne(l: LocalList[Int]): LocalList[Int] =
//  l match {
//  case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
//  case LocalCons(x, LocalNil) => LocalCons(x+1, LocalNil)
//  case LocalCons(x, xs) => LocalCons(x+1, mapForPlusOne(xs))
//  }
  foldRight(l, LocalNil:LocalList[Int])((x, y) => LocalCons(x+1, y))

mapForPlusOne(list)

/*
* EXERCISE 17: Write a function that turns each value in a List[Double] into a String.
* */

def mapDoubleToString(l: LocalList[Double]): LocalList[String] =
//  l match {
//  case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
//  case LocalCons(x, LocalNil) => LocalCons(x.toString, LocalNil)
//  case LocalCons(x, xs) => LocalCons(x.toString, mapDoubleToString(xs))
//  }
  foldRight(l, LocalNil:LocalList[String])((x, y) => LocalCons(x.toString, y))

/*
* EXERCISE 18: Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list.
* */

def map[A,B](l: LocalList[A])(f: A => B): LocalList[B] =
//  l match {
//  case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
//  case LocalCons(x, LocalNil) => LocalCons(f(x), LocalNil)
//  case LocalCons(x, xs) => LocalCons(f(x), map(xs)(f))
//  }
  foldRight(l, LocalNil:LocalList[B])((x, y) => LocalCons(f(x), y))

/*
* EXERCISE 19: Write a function filter that removes elements from a list unless they satisfy a given predicate.
* Use it to remote all odd numbers from a List[Int].
* */

def filter[A](l: LocalList[A])(f: A => Boolean):LocalList[A] =
  foldRight(l, LocalNil:LocalList[A])((x, y) => if(f(x)) LocalCons(x, y) else y)

filter(LocalList(1,2,3,4,5,6,7,8))(_%2==1)

/*
* EXERCISE 20: Write a function flatMap, that works like map except that the function given will return a list instead of a single result, and that list should be inserted into the final resulting list.
* */

def concat[B](a:LocalList[B], b:LocalList[B]):LocalList[B] =
  foldRight(a, b)((x, y) => LocalCons(x, y))

def flatMap[A,B](l: LocalList[A])(f: A => LocalList[B]): LocalList[B] = {

  foldRight(l, LocalNil:LocalList[B])((x, y) => concat(f(x), y))
}


/*
* EXERCISE 21: Can you use flatMap to implement filter?
* */

def filterByFlatMap[A](l: LocalList[A])(f: A => Boolean):LocalList[A] = {
  flatMap(l)(x => if(f(x)) LocalNil else LocalCons(x, LocalNil))
}

filterByFlatMap(LocalList(1,2,3,4,5,6,7,8))(_%2==1)

/*
* EXERCISE 22: Write a function that accepts two lists and constructs a new list by adding corresponding elements.
* For example, List(1,2,3) and List(4,5,6) becomes List(5,7,9).
* */

def zipWithInt(l:LocalList[Int], r:LocalList[Int]): LocalList[Int] = {

  @tailrec
  def loop(l: LocalList[Int], r: LocalList[Int], acc:LocalList[Int]):LocalList[Int] = (l, r) match {
    case (LocalCons(x, xs), LocalCons(y, ys)) => loop(xs, ys, LocalCons(x+y, acc))
    case _ =>  acc
  }

  loop(l, r, LocalNil)
}
/*
* EXERCISE 23: Generalize the function you just wrote so that it's not specific to integers or addition.
* */

def zipWith[A, B, C](l:LocalList[A], r:LocalList[B])(f: (A, B) => C): LocalList[C] = {
  @tailrec
  def loop(l: LocalList[A], r: LocalList[B], acc:LocalList[C]):LocalList[C] = (l, r) match {
    case (LocalCons(x, xs), LocalCons(y, ys)) => loop(xs, ys, LocalCons(f(x, y), acc))
    case _ => acc
  }
  loop(l, r, LocalNil)
}


/*
* EXERCISE 24 (hard): As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
* For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
* You may have some difficulty finding a concise purely functional implementation that is also efficient.
* That's okay. Implement the function however comes most naturally.
* We will return to this implementation in a couple of chapters and hopefully improve on it.
* Note: any two values, x, and y, can be compared for equality in Scala using the expression x == y.
* */

def hasSubsequence[A](l: LocalList[A], sub: LocalList[A]): Boolean = {

  def isSame(l:LocalList[A], r:LocalList[A]): Boolean = (l, r) match {
    case (_, LocalNil) => true
    case (LocalNil, _) => false
    case (LocalCons(x, xs), LocalCons(y, ys)) => if(x == y) isSame(xs, ys) else false
  }

  @tailrec
  def loop(a: LocalList[A]):Boolean = a match {
    case LocalNil => false
    case LocalCons(x, xs) => if(isSame(a, sub)) true else loop(xs)
  }

  loop(l)
}

val a = LocalList(1, 2, 3, 4, 5, 6, 7)
hasSubsequence(a, LocalList(1,3,4))
hasSubsequence(a, LocalList(1))
hasSubsequence(a, LocalList(1,2))
hasSubsequence(a, LocalList(3,4))
hasSubsequence(a, LocalList(5,4))
hasSubsequence(a, LocalList(1,4))
hasSubsequence(a, LocalList(1,2,3,4,5,6,7))



sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


var tree = Branch(
  Branch(
    Branch(Leaf(1), Leaf(2)), Branch(Leaf(8), Leaf(30))),
  Branch(
    Branch(
      Branch(
        Branch(Leaf(35), Leaf(40)),
        Branch(Leaf(90), Leaf(5))),
      Branch(Leaf(34), Leaf(35))),
    Leaf(1999))
)


/*
* EXERCISE 25: Write a function size that counts the number of nodes in a tree.
* */

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + size(l) + size(r)
}

size(tree)

/*
* EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int].
* (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
* */

def max(t: Tree[Int]):Int = t match {
  case Leaf(v) => v
  case Branch(l, r) => max(l) max max(r)
}

max(tree)

/*
* EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf.
* */

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => (depth(l)+1) max (depth(r)+1)
}

depth(tree)
/*
* EXERCISE 28: Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
* */

def map[A, B](t: Tree[A])(f:A => B):Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
}

map(tree)(_*10)

/*
* EXERCISE 29: Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
* Reimplement them in terms of this more general function.
* Can you draw an analogy between this fold function and the left and right folds for List?
* */

def fold[A, B](t: Tree[A])(f:A => B)(g:(B, B) => B): B = t match {
  case Leaf(value) => f(value)
  case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

object Fold{
  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_+_+1)
  def max(t: Tree[Int]):Int = fold(t)((v) => v)(_ max _)
  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)(_+1 max _+1)
  def map[A, B](t: Tree[A])(f:A => B):Tree[B] = fold(t)(v => Leaf(f(v)):Tree[B])(Branch(_,_))
}

Fold.size(tree)
Fold.max(tree)
Fold.depth(tree)
Fold.map(tree)(_*10)


