
object Solution27 {
  val primeNumbers = collection.mutable.Set[Int]()

  def main(args: Array[String]): Unit = {
    val prime = for(a <- -999 to 999; b <- -999 to 999) yield {
      (primesForQuadraticForm(a, b), a, b)
    }

    val max = prime.maxBy(_._1)
    println(s"$max, product: ${max._2*max._3}")
  }

  def primesForQuadraticForm(a: Int, b: Int): Int = {
    val start = Stream.from(0).map{ n =>
      n*n + a*n + b
    }

    start.takeWhile(isPrime(_)).length
  }

  def isPrime(number: Int): Boolean = {
    if(primeNumbers.contains(number)){
      true
    }else{
      if(number < 2){
        false
      }else {
        for (divisor <- 2 to math.sqrt(number).toInt) {
          if (number % divisor == 0) {
            return false
          }
        }

        primeNumbers += number
        true
      }
    }
  }
}
