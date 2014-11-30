
object Solution40 {
  val digits = calcCounts(6)

  def main(args: Array[String]): Unit = {
    println(digits)

    val numbers = 0 to 6 map { math.pow(10, _).toInt} map { getNumber(_) }
    println(numbers.reduce(_ * _))

  }

  def calcCounts(n: Int): List[Int] = {
    1 to n map {
      d => ((d+1)*math.pow(10, d) - (math.pow(10, d+1) -1)/9).toInt
    } toList
  }

  def getNumber(n: Int): Int = {
    val numDigits = digits.indexWhere( n < _) + 1
    val offset = if(numDigits < 2) 0 else digits(numDigits-2)

    val rest = n - offset - 1
    val number = (rest / numDigits + math.pow(10, numDigits - 1)).toInt
    val digit = rest % numDigits

    number.toString.split("").map(_.toInt).apply(digit)
  }
}
