def test(): Unit ={
  var count = 0

  val inc = () => count + 1

  inc()

  println(count)

  inc()

  println(count)
}

var count = 0

val inc = () => count + 1

inc()

println(count)

inc()

println(count)