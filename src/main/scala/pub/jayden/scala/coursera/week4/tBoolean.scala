package pub.jayden.scala.coursera.week4

/**
  * Created by jaydenuk on 2016. 4. 10..
  */
abstract class tBoolean {

  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: => tBoolean): tBoolean = ifThenElse(x, FALSE)
  def ||(x: => tBoolean): tBoolean = ifThenElse(TRUE, x)
  def unary_!():tBoolean = ifThenElse(FALSE, TRUE)

  def == (x: tBoolean): tBoolean = ifThenElse(x, x.unary_!)
  def != (x: tBoolean): tBoolean = ifThenElse(x.unary_!, x)

  def < (x: tBoolean): tBoolean = ifThenElse(FALSE, x)
}


object TRUE extends tBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = t
}

object FALSE extends tBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = e
}
