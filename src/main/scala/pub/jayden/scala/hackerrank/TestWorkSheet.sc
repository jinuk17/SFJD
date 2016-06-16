import scala.math.pow

def f(num:Int) : List[Int] = {
  def g(x:Int): List[Int] ={
    if(x <= num) x :: g(x+1)
    else Nil
  }
  g(1)
}

f(4)



def reverse(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: List() => List(y)
  case y :: ys => (reverse(ys)) ++ List(y)
}

def g(arr: List[Int]): Int = arr match {
  case Nil => 0
  case x::xs => if(x%2 == 0) g(xs) else x + g(xs)
}

g(List(3,2,4,6,5,7,8,0,1))
g(f(10))


def length(arr:List[Int]): Int = arr match {
  case List() => 0
  case x::List() => 1
  case x::xs => 1 + length(xs)
}

def f(arr:List[Int]):List[Int] = {
  arr.map(x => if(x > 0) x  else -x)
}

length(List(3,2,4,6,5,7,8,0,1))

f(List(-3,2,4,-6,5,7,-8,0,-1))

def z(x: Float):  Float= {
  def calculator(i: Int): Float = i match {
    case 1 => 1f
    case _ => (power(i-1) / factorial(i-1)) + calculator(i-1)
  }
  def factorial(i: Int): Int = {
    if (i > 1) i * factorial(i - 1) else i
  }
  def power(i: Int): Float = {
    if (i > 1) x * power(i - 1) else x
  }
  calculator(10)
}
z(-5.0000f)



def fff(coefficients:List[Int], powers:List[Int], x:Double):Double = {

  (coefficients zip powers).map(t => t._1 * pow(x, t._2)).sum

}

def area(coefficients:List[Int],powers:List[Int],x:Double):Double = {
  math.Pi*pow(fff(coefficients, powers, x), 2)
}

def summation(func:(List[Int],List[Int],Double)=>Double,upperLimit:Int,lowerLimit:Int,coefficients:List[Int],powers:List[Int]):Double = {
  // Fill up this function
  (lowerLimit*1d to upperLimit*1d by 0.001d).map(0.001d*func(coefficients,powers,_)).sum;
}


area(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), 1d)
area(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), 4d)

def isPrime(n: Int):Boolean = (2 until n) forall (d => n % d != 0)


def isPrime(n: Int):Boolean = (2 until n) forall (d => n % d != 0)

