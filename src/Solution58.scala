
object Solution58 {
  val diagonalCache = scala.collection.concurrent.TrieMap[Int, List[Int]]()

  def main(args: Array[String]): Unit = {
    val start = (0, 1, 1, 1)
    var value = nextEntry(start)

    while(value._1.toDouble/value._2 >= 0.1) {
      value = nextEntry(value)
    }

    println(value)
  }

  def diagonalNumbers(length: Int): List[Int] = {
    diagonalCache.get(length) match {
      case Some(diagonals) => diagonals
      case None => {
        val result = if(length == 1) {
          List(1)
        } else {
          val diagonals = diagonalNumbers(length-2)

          val last = diagonals.head

          val topRight = last + (length - 1)
          val topLeft = topRight + (length - 1)
          val bottomLeft = topLeft + (length -1)
          val bottomRight = bottomLeft + (length -1)

          bottomRight :: bottomLeft :: topLeft :: topRight :: diagonals
        }

        diagonalCache += length -> result

        result
      }
    }
  }

  def nextEntry(entry: (Int, Int, Int, Int)): (Int, Int, Int, Int) = {
    val (primes, entries, length, lastNumber) = entry

    val newLength = length + 2

    val topRight = lastNumber + (newLength - 1)
    val topLeft = topRight + (newLength - 1)
    val bottomLeft = topLeft + (newLength -1)
    val bottomRight = bottomLeft + (newLength -1)

    val newPrimes = List(topRight, topLeft, bottomLeft, bottomRight).map(x => if(Helper.isPrime(x.toLong)) 1 else 0).sum + primes


    (newPrimes, entries + 4, newLength, bottomRight)
  }

  def primeDensityOfLength(length: Int): Double = {
    primeDensity(diagonalNumbers(length))
  }

  def primeDensity(numbers: List[Int]): Double = {
    val numPrimes = numbers map (x => if(Helper.isPrime(x.toLong)) 1 else 0) sum

    numPrimes.toDouble / numbers.length
  }
}
