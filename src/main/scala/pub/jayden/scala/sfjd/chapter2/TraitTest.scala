package pub.jayden.scala.sfjd.chapter2

/**
 * Created by jaydenuk on 2016. 2. 25..
 */
object TraitTest {

  def main(args: Array[String]) {

        val point1 = new Point(1, 2) with Equal
        val point2 = new Point(1, 2) with Equal
        point1.isEqual(point2)
        point1.isNotEqual(point2)

    val abc = new A with C with B

    val a = new A
    println(a.print)
    println("----")
    println(abc.print)


    class Iter extends StringIterator("ABCDEFGHIJKLMNOP") with RichIterator

    val iter:RichIterator = new Iter

    iter.foreach(println)

    iter foreach println

    val cat = new Cat
    cat.print
  }

  trait Equal{
    def isEqual(x:Any): Boolean
    def isNotEqual(x:Any): Boolean = !isEqual(x)
  }

  trait GreaterThan{
    def isGreaterThan(x:Any):Boolean
  }

  class Point(xc: Int, yc: Int) extends Equal with GreaterThan{
    val x = xc
    val y = yc
    override def isEqual(obj: Any): Boolean = {
      obj.isInstanceOf[Point] &&
      obj.asInstanceOf[Point].x == x
    }

    override def isGreaterThan(obj: Any): Boolean = {
      obj.isInstanceOf[Point] &&
        obj.asInstanceOf[Point].x < x &&
        obj.asInstanceOf[Point].y < y
    }
  }

// ====================================================


  class A {
    def print = println("CLASS A");
  }

  trait B extends A {
    override def print = {
      super.print
      println("CLASS B")
    };
  }

  trait C extends A {
    override def print = {
      super.print
      println("CLASS C")};
  }

  // ====================================================


  abstract class AbsIterator{
    type T
    def hasNext:Boolean
    def next:T
  }


  trait RichIterator extends AbsIterator{
    def foreach(f: T => Unit){while(hasNext) f(next)}
  }

  class StringIterator(s:String) extends AbsIterator{

    private var i = 0

    override type T = Char

    override def next: T = {val ch = s charAt i; i += 1; ch}

    override def hasNext: Boolean = i < s.length
  }

  // ====================================================

  class Animal{ def print = println("Animal")}
  trait Furry extends Animal {override def print = {super.print; println("Furry")}}
  trait HasLegs extends Animal {override def print = {super.print; println("HasLegs")}}
  trait FourLegged extends HasLegs {override def print = {println("FourLegged")}}
  class Cat extends Animal with Furry with FourLegged {override def print = {super.print; println("Cat")}}
}
