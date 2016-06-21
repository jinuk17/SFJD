
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
    if(as.isEmpty) LocalNil
    else LocalCons(as.head, apply(as.tail: _*))
}

val list:LocalList[Int] = LocalList(1, 2, 3, 4, 5)

val x = list match {
  case LocalCons(x, LocalCons(2, LocalCons(4, _))) => x
  case LocalNil => 42
  case LocalCons(x, LocalCons(y, LocalCons(3, LocalCons(4, _)))) => x + y
  case LocalCons(h, t) => h + LocalList.sum(t)
  case _ => 101
}

val y:PartialFunction[LocalList[Int], Int] = {
  case LocalCons(x, LocalCons(2, LocalCons(4, _))) => x
  case LocalNil => 42
  case LocalCons(x, LocalCons(y, LocalCons(3, LocalCons(4, _)))) => x + y
  case LocalCons(h, t) => h + LocalList.sum(t)
  case _ => 101
}