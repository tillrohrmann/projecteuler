
object Solution43 {
  val divisors = List(2,3,5,7,11,13,17)

  def main(args: Array[String]): Unit = {
    val substringDivisable = createPandigitalNumbers(9).filter(isSubStringDivisable(_))
    println(substringDivisable.map{
      x => x.foldLeft(0l)((result, elem) => result* 10 + elem)
    }.reduce(_ + _))
  }

  def isSubStringDivisable(n: Long): Boolean = {
    val digits = Helper.digits(n).toList

    isSubStringDivisable(digits)
  }

  def isSubStringDivisable(digits: List[Int]): Boolean = {
    val slices = 1 until (1 + divisors.length) map (start => digits.slice(start, start+3)) map {
      x => x.foldLeft(0)((result, elem) => result * 10 + elem)
    }

    slices.zip(divisors).forall{case (n, d) => n%d == 0}
  }

  def createPandigitalNumbers(n: Int): List[List[Int]] = {
    (0 to n permutations).map(_.toList).toList
  }
}
