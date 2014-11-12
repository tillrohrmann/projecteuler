
object Solution21 {
  def main(args: Array[String]): Unit = {
    val sums = 2 until 10000 map { x => (x,sumOfDivisors(x)) }

    val amicablePairs = sums flatMap {
      a => {
        sums filter {
          b => {
            a._2 == b._1 && a._1 == b._2 && a._1 != b._1
          }
        }
      }
    }

    println(amicablePairs.map(_._1).reduce(_ + _))
  }

  def sumOfDivisors(n: Int): Int = {
    val divisors = getDivisors(n)

    divisors reduce (_ + _)
  }

  def getDivisors(n: Int): IndexedSeq[Int] = {
    1 to n/2 filter ( n % _ == 0)
  }
}
