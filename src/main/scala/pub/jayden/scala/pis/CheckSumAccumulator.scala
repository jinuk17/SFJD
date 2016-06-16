package pub.jayden.scala.pis

import scala.collection.mutable.Map

/**
  * Created by jaydenuk on 2016. 4. 5..
  */
class CheckSumAccumulator {

  private var sum = 0
  def add(b: Byte) { sum += b}
  def checksum(): Int = ~(sum & 0xFF) + 1
}

object  CheckSumAccumulator{

  private val cache = Map[String, Int]()

  def calculate(s: String): Int =
    if(cache.contains(s))
      cache(s)
    else {
      val acc = new CheckSumAccumulator
      for(c <- s)
        acc.add(c.toByte)
      val cs = acc.checksum()
      cache += (s -> cs)
      cs
    }

}
