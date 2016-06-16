package pub.jayden.scala.codeport.island

import org.scalatest.FunSuite
import island.{survivalTime, listToFeed}
/**
 * Created by jaydenuk on 2016. 3. 7..
 */
class islandTest extends FunSuite{

  test("Sample #1"){
    assert(survivalTime(listToFeed(List((3, 4), (0, 4), (4, 4)))) == 8)
  }

  test("Sample #2"){
    assert(survivalTime(listToFeed(List((3, 9),(0, 4), (4, 4)))) == 9)
  }
}
