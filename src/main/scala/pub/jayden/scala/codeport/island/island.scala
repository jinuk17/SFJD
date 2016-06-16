package pub.jayden.scala.codeport.island

import scala.collection.mutable.ListBuffer

/**
 * Created by jaydenuk on 2016. 3. 7..
 */
object island extends App{

  case class Food(period:Int, survive:Int){
    val valid = period + survive
    def == (other:Food) : Boolean = (this.period == other.period) && (this.survive == other.survive)
    override def toString = "[p:" + period + "/s:" + survive + "]"
  }

  def listToFeed(foodList: List[(Int, Int)]) : List[Food] = foodList.map(f => Food(f._1, f._2))

  def survivalTime(foodList:List[Food]) : Int= {

    def accSurvivalTime(rf:List[Food], survivalTime:Int) : Int = {
      val ff = rf.filter(survivalTime <= _.period)
      if(ff.isEmpty) return survivalTime
      (for{
        f<-ff
        st = accSurvivalTime(ff diff List(f), survivalTime + f.survive)
      } yield st).max
    }
    (for{
      f <- foodList
      st = accSurvivalTime(foodList diff List(f), f.survive)
    } yield st).max
  }


  def process(lineIn : Iterator[String])(lineOut: String => Unit) =
    for (i <- 1 to lineIn.next().toInt) {
      val fc = lineIn.next().toInt
      val fl = new ListBuffer[(Int, Int)]
      for (f <- 0 until fc) {
        val Array(p, s) = lineIn.next().split(' ').map(_.toInt)
        fl append Tuple2(p, s)
      }
      lineOut(s"Case #$i: ${survivalTime(listToFeed(fl.toList))}")
    }

  val path = "/Users/jaydenuk/Downloads/output/"
  val filename = "A-large-practice"
  val writer = new java.io.PrintWriter(path+filename + ".out")
  try {
    process(io.Source.fromFile(path+filename + ".in").getLines) { s =>
      writer.println(s); writer.flush()
    }
  } finally {
    writer.flush(); writer.close()
  }
}
