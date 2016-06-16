package pub.jayden.scala.codeport.codysjam

object CodysJam extends App{

  def solve(jams: List[Long], result: List[Long]) : List[Long] = {
    if (jams.size > 0) {
      solve(jams.tail.diff(List(jams.head * 100 / 75)), result:::List(jams.head))
    }
    result
  }

  def process(lineIn: Iterator[String])(lineOut: String => Unit) = {

    for(i <- 1 to lineIn.next().toInt){
      lineIn.next()
      val jams = lineIn.next().split(' ').map(_.toLong).toList
      val result = solve(jams, List[Long]()).mkString(" ")
      lineOut(s"Case #$i: $result")
    }
  }


//  val filename = "data/codysjam/example"
//  val filename = "data/codysjam/A-small-practice"
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
