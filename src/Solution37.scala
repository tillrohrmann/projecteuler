
object Solution37 {
  val primes = collection.mutable.Set[Int]()

  def main(args: Array[String]): Unit = {
    val numbers = 10 to 1000000 filter (isTruncatable(_))
    println(numbers.reduce(_ + _))
  }

  def isTruncatable(n: Int): Boolean = {
    if(isPrime(n)){
      val digits = n.toString.split("").map(_.toInt)

      var counter = 1

      while(counter < digits.length){
        val left = counter until digits.length map { digits(_) } mkString("") toInt
        val right = 0 until (digits.length - counter) map { digits(_) } mkString("") toInt

        if(!isPrime(left) || !isPrime(right)){
          return false
        }

        counter += 1
      }

      true
    }else{
      false
    }

  }

  def isPrime(n: Int): Boolean = {
    if (n > 1) {

      if (primes.contains(n)) {
        true
      } else {
        val max = math.floor(math.sqrt(n)).toInt

        if ((2 to max) forall (n % _ != 0)) {
          primes += n
          true
        } else {
          false
        }
      }
    } else {
      false
    }
  }
}
