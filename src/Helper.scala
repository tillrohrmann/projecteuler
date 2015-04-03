
object Helper {
  val primes = collection.mutable.Set[Int]()
  var maxPrime = 0
  def isPrime(n: Int): Boolean = {
    if(primes.contains(n)){
      true
    }else{
      val max = math.floor(math.sqrt(n)).toInt

      if((2 to max) forall  (n % _ != 0)){
        primes += n
        if(maxPrime < n) {
          maxPrime = n
        }
        true
      }else{
        false
      }
    }
  }

  def primeFactors(value: Long): Set[Long] = {
    var factor = 2
    var result = Set[Long]()
    var v = value

    while(v > 1) {
      while (!isPrime(factor)) {
        factor += 1
      }

      var t = 1
      while (v % factor == 0) {
        t *= factor
        v /= factor
      }

      if(t != 1) {
        result += t
      }
      factor += 1
    }

    result
  }

  def numberDigits(n: Long): Int = {
    var result = 0
    var number = if(n <0) -n else n

    while(number > 0){
      number /= 10
      result += 1
    }

    result
  }

  def digits(n: Long): List[Int] = {
    var result = List[Int]()
    var number = if(n < 0) -n else n

    while(number > 0){
      result ::= (number % 10).toInt
      number /= 10
    }

    result
  }
}
