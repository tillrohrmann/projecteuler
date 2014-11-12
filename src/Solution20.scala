
object Solution20 {
  def main(args: Array[String]): Unit = {
    val f = factorial(100)

    println(f.toString().split("").map{_.toInt}.reduce(_ + _))
  }

  def factorial(n: Int): BigInt = {
    var result = BigInt(1)

    for(i <- 1 to n){
      result *= BigInt(i)
    }

    result
  }
}
