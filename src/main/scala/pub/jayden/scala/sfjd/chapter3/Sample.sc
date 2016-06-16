import scala.annotation.tailrec

object session {
  1 + 2

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrIter(1.0)
  }


  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  val x = 1;
  val y = x + 1;
  y * y

  def loop:Int = loop

  def and(x:Boolean, y:Boolean): Boolean ={
    if(x) y else false
  }

  def and1(x:Boolean, y: => Boolean): Boolean ={
    if(x) y else false
  }

  and(true, true)


  def factorial(n: Integer): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if(n == 0) acc else loop(acc*n, n-1)
    loop(1, n)
  }

  factorial(5)


  object exercise{
    def product(f: Int => Int)(a: Int, b: Int): Int =
      if(a>b) 1 else f(a) * product(f)(a+1, b)

    def factorial(a: Int): Int = product(x => x)(1, a)
  }



  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if(a>b) acc
      else loop(a+1, f(a)+acc)
    }
    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if(a>b) 1 else f(a) * product(f)(a+1, b)

  def calculator(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a:Int, b:Int): Int ={
    if(a>b) zero
    else combine(f(a), calculator(f, combine, zero)(a+1, b))
  }

  def productA(f: Int => Int)(a: Int, b: Int): Int = calculator(f, (x, y) => x * y, 1)(a, b)

  def sumA(f: Int=> Int)(a:Int, b:Int):Int = calculator(f, (x, y) => x + y, 0)(a, b)

  sum(x=>x)(1, 5)
  sumA(x=>x)(1, 5)

}