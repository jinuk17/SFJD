

def squareList(xs: List[Int]):List[Int] = xs match {

  case Nil => xs
  case y :: ys => y*y :: squareList(ys)
}

def squareList2(xs: List[Int]):List[Int] = xs.map( x => x*x)




def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil      => Nil
  case x :: xs1 => {
    val col = xs.span( y => y == x)
    col._1 :: pack(col._2)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

val testData = List("a", "a", "a", "b", "c", "c", "a")
pack(testData)
encode(testData)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, n) => n+1 )


mapFun[String, Int](testData, _.length)

lengthFun(testData)