trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true
  override def tail: List[T] = throw new NoSuchElementException("Nil.head")
  override def head: T = throw new NoSuchElementException("Nil.tail")
}

object List{
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil[T]))
  def apply[T](): List[T] = new Nil[T]
}




object TRUE extends FBoolean{
  override def ifThenElse[T](t: => T, e: => T): T = t
}

object FALSE extends FBoolean{
  override def ifThenElse[T](t: => T, e: => T): T = e
}

abstract class FBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Boolean): Boolean = ifThenElse(x, false)
  def || (x: => Boolean): Boolean = ifThenElse(true, x)
  def unary_! : Boolean = ifThenElse(false, true)

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)


  def < (x: Boolean): Boolean = ifThenElse(false, x)
}

abstract class Nat{
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  override def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0.predecessor")
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat{
  override def isZero: Boolean = false
  def predecessor: Nat = n
  override def +(that: Nat): Nat = new Succ(n + that)
  override def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
}

