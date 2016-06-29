


object OptionMain {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)

    def flatMap1[B](f: A => Option[B]): Option[B] = {
      val a = getOrElse(None)
      if(a == None) None else f(a.asInstanceOf[A])
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map((x:A) => Some(x)).getOrElse(ob)
    }

    def orElse1[B >: A](ob: => Option[B]): Option[B] = {
      if(getOrElse(None) == None) ob else this
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap((x:A) => if(f(x)) Some(x) else None)
    }

    def filter1(f: A => Boolean): Option[A] = {
      val a = getOrElse(None)
      if(a == None|| f(a.asInstanceOf[A])) None else this
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}


import OptionMain._

//Exercise 2
def variance(xs: Seq[Double]): Option[Double] = {

  def average(ys: Seq[Double]): Option[Double] = {
    if (ys.isEmpty) None
    else Some(ys.sum / ys.size)
  }

  def calculate(xs: Seq[Double])(m: Double): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.foldLeft(0.0)((a, b) => a + math.pow(b + m, 2)) / xs.size)
  }

  //average(xs).flatMap(calculate(xs)_)

  average(xs).flatMap(x => average(xs.map( y => math.pow(y + x, 2))))
}

variance(Seq(1,2,3,4,5,6,7,7))