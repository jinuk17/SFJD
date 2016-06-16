

object FileMatcher {
  private def filesHere = (new java.io.File(".")).listFiles

  def filesMatching(matcher: (String) => Boolean) =
    for(file <- filesHere; if matcher(file.getName)) yield file

  def filesEnding(query: String) = filesMatching(_.endsWith(query))
  def filesContaining(query: String) = filesMatching(_.contains(query))
  def filesRegex(query: String) = filesMatching(_.matches(query))
}


def curriedSum(x: Int)(y: Int)  = x + y

val plusThree = curriedSum(3)_
