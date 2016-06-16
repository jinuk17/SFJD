import scala.annotation.tailrec

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
}

var x: List[Int] = List();

x = insert(2, x)
x = insert(2, x)

def f(num:Int,arr:List[Int]):List[Int] = for(n <- arr; m <- 1 to num) yield n


f(3, List(10,6,7,5,7,1,2,3,4))



def g(delim:Int,arr:List[Int]):List[Int] = arr.filter(x => x < delim)

g(25, List(25 ,-41,46,-28,21,52,83,-29,84,27,40))


def h(arr:List[Int]):List[Int] = {
  @tailrec
  def removeOddElement(temp: List[Int], l : List[Int], isOddPosition: Boolean): List[Int] = l match {
    case Nil => temp
    case x :: xs => {
      if(isOddPosition) removeOddElement(temp, xs, false)
      else removeOddElement(x :: temp, xs, true)
    }
  }
  removeOddElement(List(), arr, true).reverse
}
def f(arr:List[Int]):List[Int] = {
  def removeOddElement(temp: List[Int], l : List[Int], isOddPosition: Boolean): List[Int] = l match {
    case Nil => temp
    case x :: xs => {
      if(isOddPosition) removeOddElement(temp, xs, false)
      else removeOddElement(x :: temp, xs, true)
    }
  }
  removeOddElement(List(), arr, true).reverse
}


object Solution extends App {
  def f(arr:List[Int]):List[Int] = {
    def removeOddElement(temp: List[Int], l : List[Int], isOddPosition: Boolean): List[Int] = l match {
      case Nil => temp
      case x :: xs => {
        if(isOddPosition) removeOddElement(temp, xs, false)
        else removeOddElement(x :: temp, xs, true)
      }
    }
    removeOddElement(List(), arr, true).reverse
  }
  println(f(io.Source.stdin.getLines.toList.map(_.trim).map(_.toInt)).mkString("\n"))
}


val a = List(1,2,3,4)
val b = List(5,6,7,8)
def init[T](xs: List[T]): List[T] = xs match{
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(init(a))
a ::: b
a ++ b

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]):List[T] = xs match {
  case List() => List()
  case y :: ys => {
    if(n == 0) ys else y::removeAt(n-1, ys)
  }
}

def removeAt2[T](n: Int, xs: List[T]):List[T] = (xs take n) ::: (xs drop n+1)

removeAt(2, a)
removeAt2(2, a)

def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => xs
  case (y:List[Any]) :: ys => flatten(y) ::: flatten(ys)
  case y :: ys => y :: flatten(ys)
}

flatten(List(1,2,List('a','b','c'),3, List(4,5, List('y','z'))))

