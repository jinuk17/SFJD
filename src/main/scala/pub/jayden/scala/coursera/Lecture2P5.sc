import scala.annotation.tailrec

object rationals{

}
implicit def intToDoubleRational(x: Int) = new Rational(x) add new Rational(x)
class Rational(x:Int, y:Int){

  require(y != 0, "denominator must be nonzero")

  private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b, a % b)
  def numer = x
  def denom = y

  def this(x:Int) = this(x, 1)

  def less(that : Rational) = numer * that.denom < that.numer * denom
  def max(that : Rational) = if(this.less(that)) that else this

  def add(that: Rational) =
    new Rational(numer *that.denom + that.numer * denom, denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)
y.add(y)

2 add new Rational(4)


class Factorial2 {

  def factorial(n: Int): Int = {

    @tailrec
    def factorialAcc(acc: Int, n: Int): Int = {
      if (n <= 1) acc
      else factorialAcc(n * acc, n - 1)
    }
    factorialAcc(1, n)
  }

  def factorial2(n: Int): Int = {
      if (n <= 1) n
      else factorial2(n - 1) + n
  }
}

case class Logged[T](value:T, log:List[String])
def initLogged(x:Int):Logged[Int] = Logged(x, List())

def doubleLogged(x:Int):Logged[Int] = Logged(x+x, List("double("+x+") = " + (x+x)))
def sqrtLogged(x:Int):Logged[Int] = Logged(Math.sqrt(x).toInt, List("sqrt("+x+").toInt = " + Math.sqrt(x).toInt))

def o[T,V,U](f:T=>V, g:V=>U) = (x:T) => g(f(x))

def mkLoggedFun(f:Int=>Logged[Int]) = (x:Logged[Int]) => {
  val value = x.value    // x(로그 포함된 값)에서 내부의 값 노출시키기
  val value2 = f(value)  // 함수 적용
  value2                 // 값 반환
}

def mkLoggedFunRevised(f:Int=>Logged[Int]) = (x:Logged[Int]) => {
  val value = x.value    // x(로그 포함된 값)에서 내부의 값 노출시키기
  val log = x.log        // x에서 로그 가져오기
  val value2 = f(value)  // 함수 적용
  Logged(value2.value, log:::value2.log)
}
val x3  = mkLoggedFunRevised(doubleLogged)(initLogged(10))
val x4 = o(mkLoggedFunRevised(doubleLogged), mkLoggedFunRevised(sqrtLogged))(initLogged(8))


abstract class MyOption[+A]

case class MySome[+A](x: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

def doubleMyOption(x:Int):MyOption[Int] = MySome(x+x)
def sqrtMyOption(x:Int):MyOption[Int] = if(x>=0) MySome(Math.sqrt(x).toInt) else MyNone

def mkMyOptionFun(f: (Int=>MyOption[Int])) = (x: MyOption[Int]) => x match {
  case MySome(x) => {
    val value2 = f(x)
    value2
  }
  case MyNone => MyNone
}

val x5 = o(mkMyOptionFun(doubleMyOption), mkMyOptionFun(sqrtMyOption))(MySome(8))



abstract class MyList[+A]

case class Cons[B](var hd: B, var tl: MyList[B]) extends MyList[B]

case object MyNil extends MyList[Nothing]

def initMyList(x:Int):MyList[Int] = Cons(x,MyNil)

def doubleMyList(x:Int):MyList[Int] = Cons(x+x,MyNil)
def sqrtMyList(x:Int):MyList[Int] = Cons(Math.sqrt(x).toInt,MyNil)

def mkMyListFun(f: Int => MyList[Int]) = (x: MyList[Int]) => {
  def append(l1: MyList[Int], l2:MyList[Int]): MyList[Int] = l1 match {
    case Cons(h, t) => {
      Cons(h, append(t, l2))
    }
    case MyNil => l2
  }

  def mapAll(l:MyList[Int]): MyList[Int] = l match {
    case Cons(h, t) => {
      val value2 = f(h)
      val remain = mapAll(t)
      append(value2, remain)
    }
    case MyNil => MyNil
  }
  mapAll(x)
}