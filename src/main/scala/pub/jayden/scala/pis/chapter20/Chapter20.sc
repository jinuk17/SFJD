

trait AbstractTime{
  var hour: Int
  var minute: Int
}




class Time extends AbstractTime {
  override var hour: Int = _
  override var minute: Int = _
}



trait RationalTrait{
  val numerArg: Int
  val denomArg: Int

  require(denomArg != 0)

  private val g = gcd(numerArg, denomArg)


  val numer = numerArg / g
  val denom = denomArg / g


  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }

  override def toString: String = numer + " / " +denom
}


val x = 2


new RationalTrait {
  override val denomArg: Int = 1 * x
  override val numerArg: Int = 2 * x
}