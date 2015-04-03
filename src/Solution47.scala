import scala.collection.mutable

object Solution47 {
  def main(args: Array[String]): Unit = {
    println(findConsecutiveNumbers(4))
    // 134043
  }

  def findConsecutiveNumbers(n: Int): Int = {
    val queue = mutable.Queue[Set[Long]]()

    val initial = (2 to (n+1)).map(Helper.primeFactors(_))

    var counter = n+2

    queue.enqueue(initial: _*)

    var numbersFound = false

    while(!numbersFound) {
      if(checkCondition(n, queue)) {
        numbersFound = true
      } else {
        queue.dequeue()
        queue.enqueue(Helper.primeFactors(counter))
        counter += 1
      }
    }

    counter - n
  }

  def checkCondition(n: Int, queue: mutable.Queue[Set[Long]]): Boolean = {
    queue.forall(_.size == n) && queue.reduce(_ ++ _).size == n*n
  }
}
