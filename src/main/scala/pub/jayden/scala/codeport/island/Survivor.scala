package pub.jayden.scala.codeport.island

/**
 * Created by jaydenuk on 2016. 3. 7..
 */

object Survivor extends  App{

  def maxTime(foods: Seq[(Int, Int)]): Int = {

    implicit class Food(f: (Int, Int)) { val sum: Int = f._1 + f._2 }

    def reachable(foods: List[(Int, Int)])(time: Int): Boolean = time == 0 ||
      foods.nonEmpty && 0 < time && time <= foods.head.sum && {
        reachable(foods.tail)(time - foods.head._2) || reachable(foods.tail)(time)
      }

    val sorted = foods.sortBy(-_.sum).toList

    (Stream from (sorted.head.sum, -1) filter reachable(sorted)).head
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val foods = Seq.fill(lineIn.next().toInt) {
        val Array(p, s) = lineIn.next().split(' ').map(_.toInt)
        (p, s)
      }
      lineOut(s"Case #$i: ${maxTime(foods)}")
    }

  val filename = "A-large-practice"
  val writer = new java.io.PrintWriter(filename + ".out")
  try {
    process(io.Source.fromFile(filename + ".in").getLines) { s =>
      writer.println(s); writer.flush()
    }
  } finally {
    writer.flush(); writer.close()
  }
}
