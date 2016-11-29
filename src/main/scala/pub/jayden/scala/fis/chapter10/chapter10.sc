import pub.jayden.scala.fis.chapter10.Monoid
import pub.jayden.scala.fis.chapter10._

val evenLengthList = List(1, 2, 3, 4, 5, 6)
val oddLengthList = List(1, 2, 3, 4, 5)

System.out.println(s"evenLengthList: ${evenLengthList.length / 2}")
System.out.println(s"oddLengthList: ${oddLengthList.length / 2}")

evenLengthList.slice(0, 2+1)
evenLengthList.slice(2+1, evenLengthList.length)

oddLengthList.slice(0, 2)
oddLengthList.slice(2+1, oddLengthList.length)

evenLengthList.splitAt(evenLengthList.length/2)
oddLengthList.splitAt(oddLengthList.length/2)


val M: Monoid[Map[String, Map[String, Int]]] =
  mapMergeMonoid(mapMergeMonoid(intAddition))