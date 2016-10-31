package pub.jayden.scala.hackerrank

import org.scalatest.FlatSpec

/**
  * Created by jaydenuk on 2016. 9. 23..
  */
class FunctionOrNotTest extends FlatSpec{

  val input =
    """2
       3
       1 1
       2 2
       3 3
       4
       1 2
       2 4
       3 6
       4 8
    """.stripMargin


  "input" should "be string" in {
    assert(input.length > 0)
  }

}
