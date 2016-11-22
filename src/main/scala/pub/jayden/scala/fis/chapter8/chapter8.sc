import pub.jayden.scala.fis.chapter8.{Gen, Prop}


val smallInt = Gen.choose(-10, 10)

val maxProp = Prop.forAll(Gen.listOf(smallInt)) {
  case Nil => true
  case x: List[Int] =>
    val max = x.max
    !x.exists(_ > max)
}



Prop.run(maxProp)