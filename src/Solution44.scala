import scala.collection.mutable

object Solution44 {

  def main(args: Array[String]): Unit = {
//    println(findMinimalPair)

    val pl = calcPentagonalNumber(1020)
    val pr = calcPentagonalNumber(2167)

    (isPentagonalNumber(pr-pl))
    isPentagonalNumber(pr + pl)
    println(pr - pl)
    //1020, 2167

  }

  def findMinimalPair: (Int, Int) = {
    implicit val ordering = new Ordering[(Int, Int)]() {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        val xPent=  calcPentagonalNumber(x._2) - calcPentagonalNumber(x._1)
        val yPent = calcPentagonalNumber(y._2) - calcPentagonalNumber(y._1)

        (yPent - xPent).toInt
      }
    }

    val priorityQueue = new mutable.PriorityQueue[(Int, Int)]

    priorityQueue += 1 -> 2

    var pairFound = false
    var nextPair: (Int, Int) = null
    var currentMax = 2
    var counter = 0l

    while(!pairFound) {
      nextPair = priorityQueue.dequeue()
      counter += 1

      if(counter % 10000 == 0) {
        println("Counter " + counter + " next pair" + nextPair)
      }

      val pl = calcPentagonalNumber(nextPair._1)
      val pr = calcPentagonalNumber(nextPair._2)

      if(isPentagonalNumber(pr+pl) && isPentagonalNumber(pr - pl)) {
        pairFound = true
      } else {
        if(nextPair._1 == 1) {
          priorityQueue += currentMax -> (currentMax + 1)
          currentMax += 1
        } else {
          val nextDiff = calcPentagonalNumber(currentMax + 1) - calcPentagonalNumber(currentMax)
          val nextDiffMinus = pr - calcPentagonalNumber(nextPair._1 - 1)
          priorityQueue += (nextPair._1 - 1) -> nextPair._2

          if(nextDiff < nextDiffMinus) {
            priorityQueue += currentMax -> (currentMax + 1)
            currentMax += 1
          }
        }

      }
    }

    nextPair
  }

  def calcPentagonalNumber(n: Long): Long = {
    n*(3*n-1)/2
  }

  def isPentagonalNumber(number: Long): Boolean = {
    println((math.sqrt(number* 24 + 1)/6 + 1.0/6))
    (math.sqrt(number* 24 + 1)/6 + 1.0/6) % 1 == 0
  }
}
