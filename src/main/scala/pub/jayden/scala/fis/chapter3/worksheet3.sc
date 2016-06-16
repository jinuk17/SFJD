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
    case LocalCons(0.0, _) => 0.0
    case LocalCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): LocalList[A] =
    if (as.isEmpty) LocalNil
    else LocalCons(as.head, apply(as.tail: _*))
}

def tail[A](a:LocalList[A]): LocalList[A] = a match {
  case LocalNil => throw new IllegalArgumentException("LocalNil is not supported.")
  case LocalCons(x, xs) => xs
}