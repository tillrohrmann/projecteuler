
object Solution56 {
  def main(args: Array[String]): Unit = {
    val result = for(
      a <- 1 until 100;
      b <- 1 until 100
    ) yield {
      (a, b, digitalSum(BigInt(a).pow(b)))
    }

    val max = result.maxBy(_._3)

    println(max)
  }

  def digitalSum(number: BigInt): Long = {
    Helper.digits(number).foldLeft(0l)(_ + _)
  }
}
