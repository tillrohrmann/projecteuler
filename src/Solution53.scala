
object Solution53 {
  def main(args: Array[String]): Unit = {
    val result = for(n <- 1 to 100; r <- 1 to n) yield {
      if(isGreaterThanMillion(n, r)) 1 else 0
    }

    println(result.sum)
  }

  def isGreaterThanMillion(n: Int, r: Int): Boolean = {
    var result = 1.0
    var counter = 0

    while(result <= 1000000 && counter < r) {
      result *= (n-counter).toDouble/(r-counter)
      counter += 1
    }

    result > 1000000
  }
}
