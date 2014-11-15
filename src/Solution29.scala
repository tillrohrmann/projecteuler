
object Solution29 {
  def main(args: Array[String]): Unit = {
    println(distinctTerms(100,100))
  }

  def distinctTerms(maxA: Int, maxB: Int): Int = {
    val numbers = for(a <- 2 to maxA; b <- 2 to maxB) yield math.pow(a,b)

    numbers.toSet.size
  }
}
