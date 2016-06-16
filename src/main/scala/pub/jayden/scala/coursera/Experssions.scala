package pub.jayden.scala.coursera

/**
  * Created by jaydenuk on 2016. 4. 14..
  */
class Expressions {

  trait Expr {
    def isNumber : Boolean
    def isSum : Boolean

    def numValue : Int
    def leftOp : Expr
    def rightOp : Expr
  }

  class Number(n : Int) extends Expr {
    override def isNumber: Boolean = true
    override def isSum: Boolean = false

    override def numValue: Int = n

    override def leftOp: Expr = throw new Error("Number.leftOp")
    override def rightOp: Expr = throw new Error("Number.rightOp")

  }

  class Sum(e1 : Expr, e2 : Expr) extends Expr {
    override def isNumber: Boolean = false
    override def isSum: Boolean = true

    override def numValue: Int = throw new Error("Sum.numValue")

    override def leftOp: Expr = e1
    override def rightOp: Expr = e2
  }



}
