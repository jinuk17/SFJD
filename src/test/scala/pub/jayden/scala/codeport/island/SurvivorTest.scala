package pub.jayden.scala.codeport.island

import org.scalatest.{Matchers, FunSuite}
import Survivor._

/**
 * Created by jaydenuk on 2016. 3. 7..
 */
class SurvivorTest extends FunSuite with Matchers{

  test("sample #1") {
    assert(maxTime(Seq((3, 4), (0, 4), (4, 4))) === 8)
  }

  test("sample #2") {
    assert(maxTime(Seq((3, 9), (0, 4), (4, 4))) === 9)
  }

  test("small #4") {
    val foods = List((25,12), (99,27), (22,70), (84,38), (75,64), (2,70), (6,78), (100,13), (94,62), (0,36))
    assert(maxTime(foods) === 153)
  }

  test("small #6") {
    val foods = List((41,31), (99,7), (68,84), (79,87), (55,89), (71,58), (13,87), (83,98), (80,100), (48,31))
    assert(maxTime(foods) === 169)
  }

  test("sample case") {
    val input = """2
3
3 4
0 4
4 4
3
3 9
0 4
4 4""".lines

    val expected = """Case #1: 8
Case #2: 9""".lines

    lineComparison(input, expected)
  }

  test("full small case") {
    val input = io.Source.fromFile("A-small-practice.in").getLines()
    val expected = io.Source.fromFile("A-small-practice.out").getLines()

    lineComparison(input, expected)
  }

  ignore("full large case") {
    val input = io.Source.fromFile("A-large-practice.in").getLines()
    val expected = io.Source.fromFile("A-large-practice.out").getLines()

    lineComparison(input, expected)
  }

  def lineComparison(input: Iterator[String], expected: Iterator[String]) {
    process(input) { s =>
      for (line <- s.lines) assert(line.trim === expected.next().trim)
    }
    assert(expected.hasNext === false, "Finished too fast.")
  }
}
