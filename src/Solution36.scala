import scala.collection.mutable.BitSet

object Solution36 {
  def main(args: Array[String]): Unit = {
    val numbers = 1 until 1000000 filter (x => isPalindrom10(x) && isPalindrom2(x))

    println(numbers.reduce(_ + _))
  }

  def isPalindrom10(n: Int): Boolean = {
    val r = n.toString.split("").reverse.mkString("").toInt

    n == r
  }

  def isPalindrom2(n: Int): Boolean = {
    val base2 = toBase2(n)

    base2 == base2.reverse
  }

  def toBase2(n: Int): List[Boolean] ={
    var result = List[Boolean]()
    var number = n

    while(number > 0){
      if(number % 2 != 0){
        result ::= true
      }else{
        result ::= false
      }

      number = number /2
    }

    result
  }
}
