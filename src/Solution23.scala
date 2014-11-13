
object Solution23 {

  def main(args: Array[String]): Unit = {
    val abundants = 1 to 28123 filter (isAbdundant(_))

    val abundantSums = for(a <- abundants; b <- abundants) yield {
      a + b
    }

    val abundantSumSet = abundantSums.filter(_ <= 28123).toSet

    val sum = 1 to 28123 filter ( !abundantSumSet.contains(_)) reduce (_ + _)

    println(sum)
  }

  def isAbdundant(n: Int): Boolean = {
    n < properDivisors(n).foldLeft(0)(_ + _)
  }

  def properDivisors(n: Int): IndexedSeq[Int] = {
    if(n == 1){
      IndexedSeq()
    }else {
      1 to n/2 filter (n % _ == 0)
    }
  }
}
