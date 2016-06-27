package pub.jayden.scala.fis

/**
  * Created by jaydenuk on 2016. 6. 22..
  */
object Test extends App{

  val setHeadResult = setHead(list)

}


sealed trait LocalList[+A]
case object LocalNil extends LocalList[Nothing]
case class LocalCons[+A](head: A, tail: LocalList[A]) extends LocalList[A]

