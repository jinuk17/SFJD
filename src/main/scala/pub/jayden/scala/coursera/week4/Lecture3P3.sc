import pub.jayden.scala.coursera.week4.{Cons, _}

def nth[T](n: Int, xs: List[T]): T =
  if(n == 0) xs.head
  else nth(n -1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Nil)))))

val f = (x: Int) => x*x

val ff = new Function[Int, Int] {
  def apply(x : Int) = f(x)

}


type ¬[A] = A => Nothing
type ∨[T, U] = ¬[¬[T] with ¬[U]]
type ¬¬[A] = ¬[¬[A]]
implicitly[¬¬[Int] <:< (Int ∨ String)]
implicitly[¬¬[String] <:< (Int ∨ String)]
type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

//def size[T](t: T)(implicit ev: (¬¬[T] <:< (Int ∨ String))) =
//  t match {
//    case i: Int => i
//    case s: String => s.length
//}
def size[T: (Int |∨| String)#λ](t: T) =
  t match {
    case i: Int => i
    case s: String => s.length
}

size(11)
size("foo")

