
object Solution57 {
  def main(args: Array[String]): Unit = {
    var value = (BigInt(1),BigInt(1))

    val expansions = for(i <- 1 to 1000) yield {
      value = nextExpansion(value._1, value._2)
      value
    }

    val mapped = expansions map {
      case (numerator, denominator) => Helper.digits(numerator).length > Helper.digits(denominator).length
    } map (if(_) 1 else 0)

    val result = mapped sum

    println(result)
  }

  def nextExpansion(numerator: BigInt, denominator: BigInt): (BigInt, BigInt) = {
    val t = numerator - denominator

    val newDenominator = 2*denominator + t
    val newNumerator = denominator + newDenominator

    (newNumerator, newDenominator)
  }
}
