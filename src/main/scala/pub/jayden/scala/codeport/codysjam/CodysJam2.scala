package pub.jayden.scala.codeport.codysjam

/**
  * Created by jaydenuk on 2016. 4. 6..
  */
object CodysJam2 extends App{

  def salePrices(prices: Seq[Int]): Seq[Int] =
    ((List.empty[Int], collection.immutable.Queue.empty[Int]) /: prices) {
      case ((acc, originals), price) if originals.headOption == Some(price) =>
        (acc, originals.tail)
      case ((acc, originals), price) =>
        (price::acc, originals :+ (price / 3 * 4))
    }._1.reverse

  def process(lineIn: Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val n = lineIn.next().toInt
      val prices = lineIn.next().split(' ').map(_.toInt)
      lineOut(s"Case #$i: ${salePrices(prices) mkString " "}")
    }

  val filename = "data/codysjam/A-large-practice"
  val writer = new java.io.PrintWriter(filename + ".out")
  try {
    process(io.Source.fromFile(filename + ".in").getLines) { s =>
      writer.println(s); writer.flush()
    }
  } finally {
    writer.flush(); writer.close()
  }

}
