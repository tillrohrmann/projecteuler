
object Solution30 {
  def main(args: Array[String]): Unit = {
    val t = 2 to 400000 filter { isSumOfFifthPower(_) } reduce {_ + _}

    println(t)
  }

  def isSumOfFifthPower(number: Int): Boolean = {
    number.toString.split("").map{_.toInt}.map{ x => math.pow(x,5) }.reduce{_ + _} == number
  }
}
