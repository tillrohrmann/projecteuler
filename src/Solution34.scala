
object Solution34 {
  val factorials = 0 to 9 map { x => (x, factorial(x))} toMap
  def main(args: Array[String]): Unit = {

    val result = 3 to 10000000 filter(isSumOfDigitFactorials(_))
    println(result)
  }


  def isSumOfDigitFactorials(number: Int): Boolean = {
    val digits = number.toString.split("").map{_.toInt}

    val sum = digits map { factorials(_)} reduce { _ + _ }

    sum == number
  }

  def factorial(n: Int): Int = {
    var result = 1
    var c = n

    while(c > 1){
      result *= c
      c -= 1
    }

    result
  }
}
