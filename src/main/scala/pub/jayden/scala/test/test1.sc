abstract class MyList[+A]

case class Cons[B](var hd: B, var tl: MyList[B]) extends MyList[B]

case object MyNil extends MyList[Nothing]


def initMyList(x:Int):MyList[Int] = Cons(x,MyNil)


def doubleMyList(x:Int):MyList[Int] = Cons(x+x,MyNil)
def sqrtMyList(x:Int):MyList[Int] = Cons(Math.sqrt(x).toInt,MyNil)

def mkMyListFun(f:Int=>MyList[Int]) = (x:MyList[Int]) => {
  def append(l1:MyList[Int], l2:MyList[Int]):MyList[Int] = l1 match {
    case Cons(h,t) => {
      Cons(h,append(t,l2))
    }
    case MyNil => l2
  }
  def mapAll(l:MyList[Int]):MyList[Int] = l match {
    case Cons(h,t) => {
      val value2 = f(h)
      val remain = mapAll(t)
      append(value2,remain)
    }
    case MyNil => MyNil
  }
  mapAll(x)
}

def o1[T,V,U](f:T=>V, g:V=>U) = (x:T) => g(f(x))
def o[T,V,U](f:T=>V) = (g:V=>U) => (x:T) => g(f(x))

val x1 = Cons(1,Cons(2,Cons(8,MyNil)))

val x7 = o(mkMyListFun(doubleMyList))(mkMyListFun(sqrtMyList))(x1)

