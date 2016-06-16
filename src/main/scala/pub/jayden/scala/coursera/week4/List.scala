package pub.jayden.scala.coursera.week4

/**
  * Created by jaydenuk on 2016. 4. 6..
  */


trait List[+T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}

case class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  override def isEmpty = false
}

case class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true
  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")
  override def head: T = throw new NoSuchElementException("Nil.head")
}

object Nil extends List[Nothing]{
  override def isEmpty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}
private object List{
  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, Nil))
  def apply[T](x1: T) = new Cons[T](x1, Nil)
  def apply[T]() = Nil
}

object Main extends App{

  def nth[T](n: Int, xs: List[T]): T =
    if(xs.isEmpty)
      throw new IndexOutOfBoundsException
    else if(n == 0) xs.head
    else nth(n -1, xs.tail)


//  val x: List[String] = Nill
//
//  val list = new Cons(1, new Cons(2, new Cons(3, Nill)))
//
//  nth(2, list)
//  nth(4, list)

}

