package pub.jayden.scala.hackerrank

/**
  * Created by jaydenuk on 2016. 4. 19..
  */
object Solution extends App{

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

  val a = List(1, 2,3,4,5,6,7,8,9)

  def g(arr: List[Int]): List[Int] = arr match {
    case odd::Nil => Nil
    case odd::even::Nil => even::Nil
    case odd::even::tail => even::g(tail)
  }


  def h(temp:List[Int], arr: List[Int]): List[Int] = arr match {
    case odd::Nil => temp
    case odd::even::Nil => even::temp
    case odd::even::tail => h(even::temp, tail)
  }

  println(h(List(), a).mkString(" "))

  println(f(io.Source.stdin.getLines.toList.map(x => x.trim).map(_.toInt)).mkString("\n"))
}
