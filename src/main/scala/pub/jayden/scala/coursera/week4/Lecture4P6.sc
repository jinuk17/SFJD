trait Expr {

  def eval(): Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval() + e2.eval()
  }
}
case class Number(n : Int) extends Expr
case class Var(s : String) extends Expr
case class Sum(e1 : Expr, e2 : Expr) extends Expr
case class Prod(e1 : Expr, e2 : Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Var(s) => s
  case Sum(e1, e2) => "(" + show(e1) +" + "+ show(e2) +")"
  case Prod(e1, e2) => show(e1) +" * "+ show(e2)
}
show(Sum(Prod(Number(2), Var("x")), Var("y")))

show(Prod(Sum(Number(2), Var("x")), Var("y")))


var fruit = scala.collection.immutable.List("apples", "oranges", "pears")

