
object Solution38 {
  def main(args: Array[String]): Unit = {
    val numbers = 2 to 9 flatMap(pandigital(_))

    println(numbers)

    val pandigitals = numbers.map{
      case (n, ms) => calcNumber(n, ms)
    }

    println(pandigitals)
  }


  def pandigital(n: Int): List[(Int, List[Int])] = {
    val multipliers = 1 to n toList
    val start = math.pow(10, (9.0/n -1).toInt).toInt
    val end = math.pow(10, (9.0/n).toInt).toInt

    start until end filter { x => ispandigital(calcNumber(x, multipliers)) } map { (_,
      multipliers)} toList
  }

  def calcNumber(n: Int, multiplier: List[Int]): Long = {
    multiplier map { x => (x * n).toString() } reduce { _ + _} toLong
  }

  def ispandigital(n: Long): Boolean = {
    numberDigits(n) == 9 && digits(n).groupBy(x => x).forall{
      case (n, l) =>{
        if(n == 0){
          l.length == 0
        }else{
          l.length == 1
        }
      }
    }
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
