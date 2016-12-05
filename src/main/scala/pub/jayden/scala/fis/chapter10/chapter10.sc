import pub.jayden.scala.fis.chapter10.Monoid
import pub.jayden.scala.fis.chapter10._

val evenLengthList = List(1, 2, 3, 4, 5, 6)
val oddLengthList = List(1, 2, 3, 4, 5)

System.out.println(s"evenLengthList: ${evenLengthList.length / 2}")
System.out.println(s"oddLengthList: ${oddLengthList.length / 2}")


evenLengthList.slice(0, evenLengthList.length / 2)
evenLengthList.slice(evenLengthList.length / 2, evenLengthList.length)

oddLengthList.slice(0, evenLengthList.length / 2)
oddLengthList.slice(evenLengthList.length / 2, oddLengthList.length)

evenLengthList.splitAt(evenLengthList.length/2)
oddLengthList.splitAt(oddLengthList.length/2)


val M: Monoid[Map[String, Map[String, Int]]] =
  mapMergeMonoid(mapMergeMonoid(intAddition))


val orderedList = IndexedSeq(1, 2, 3, 4, 5, 6)
isOrdered(orderedList)

val notOrderedList = IndexedSeq(2, 1, 1, 1, 1, 1, 1)
isOrdered(notOrderedList)


val testString = " ajjm s,skfs asdkasfdasj asdjasd sdjsafsasa,"

countWord(testString)

foldableList.foldLeft(evenLengthList)("AAA")((b, a) => b + a.toString)

val map: String = foldableList.foldMap(evenLengthList)(_.toString + "AAA ")(stringMonoid)

System.out.println(map)

