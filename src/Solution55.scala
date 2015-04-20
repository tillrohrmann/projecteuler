
object Solution55 {
  def main(args: Array[String]): Unit = {
    val result = 1 until 10000 map {
      n => if(isLychrel(n)) 1 else 0
    } sum

    println(result)
  }

  def isLychrel(number: BigInt): Boolean = {
    var counter = 0
    val maxIterations = 50
    var notLychrel = false
    var n = number

    while(counter < maxIterations && !notLychrel) {
      val digits = Helper.digits(n)

      n = n + Helper.bigIntFromDigits(digits.reverse)

      notLychrel = isPalindrom(n)
      counter += 1
    }

    !notLychrel
  }

  def isPalindrom(number: BigInt): Boolean = {
    val digits = Helper.digits(number)

    digits.sameElements(digits.reverse)
  }
}
