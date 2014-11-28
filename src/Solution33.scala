
object Solution33 {
  def main(args: Array[String]): Unit = {
    val fractions = for(denominator <- 11 to 99; numerator <- 10 until denominator if(simplifiable
      (numerator, denominator))) yield {
      (numerator, denominator)
    }

    val (num, denom) = fractions reduce{ (a,b) => (a._1*b._1, a._2 * b._2)}


  }

  def simplifiable(num: Int, denom: Int): Boolean = {
    val digits1 = num.toString.split("").map(_.toInt)
    val digits2 = denom.toString.split("").map(_.toInt)

    val value = num.toDouble / denom

    for(i <- 0 until digits1.length; j <- 0 until digits2.length){
      if(digits1(i) != 0 && (digits1(i) == digits2(j))){
        val newNum = digits1.zipWithIndex.filter(_._2 != i).map(_._1).mkString("").toInt
        val newDenom = digits2.zipWithIndex.filter(_._2 != j).map(_._1).mkString("").toInt

        val newValue = newNum.toDouble/newDenom

        if(newValue == value){
          return true
        }
      }
    }

    false
  }
}
