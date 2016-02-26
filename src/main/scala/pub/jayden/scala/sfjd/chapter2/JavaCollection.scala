package pub.jayden.scala.sfjd.chapter2

import java.util

import scala.collection.JavaConverters._

/**
 * Created by jaydenuk on 2016. 2. 24..
 */
object JavaCollection {

  def main(args: Array[String]) {

    val javaList = util.Arrays.asList(1,2,3,4)

    println(javaList.getClass)

    val scalaList = javaList.asScala

    println(manOf(scalaList))
    println(scalaList.getClass)


    val javaListAgain = scalaList.asJava

    println(javaListAgain.getClass)

    println(assert(javaList eq javaListAgain))

    println(assert(javaList eq scalaList))
  }

  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

}
