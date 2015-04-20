
object Helper {
  val primes = collection.mutable.Set[Long]()
  var maxPrime = 0l
  def isPrime(n: Long): Boolean = {
    if(primes.contains(n)){
      true
    }else if(n > 1){
      val max = math.floor(math.sqrt(n)).toLong

      if((2l to max) forall  (n % _ != 0)){
        primes += n
        if(maxPrime < n) {
          maxPrime = n
        }
        true
      }else{
        false
      }
    } else {
      false
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

  def digits(number: BigInt): List[Int] = {
    var result = List[Int]()
    var n = if(number < 0) -number else number

    while(n >0 ){
      result ::= (n % 10).toInt
      n /= 10
    }

    result
  }

  def numberFromDigits(digits: List[Int]): Long = {
    digits.foldLeft(0l)((result, digit) => result*10 + digit)
  }

  def bigIntFromDigits(digits: List[Int]): BigInt = {
    BigInt(digits.mkString(""))
  }
}
