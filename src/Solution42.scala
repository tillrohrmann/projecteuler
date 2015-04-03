import scala.io.Source

object Solution42 {
  val triangleNumbers = collection.mutable.Set[Int]()
  var maxTriangleNumber = 0
  var currentN = 0

  def main(args: Array[String]): Unit = {
    val wordValues = Source.fromFile("p042_words.txt").getLines().flatMap(line => line.split(",")
      .map(x => calcWordValue(x.tail.init)))
    println(wordValues.toList.filter(isTriangleNumber(_)).length)
  }

  def calcWordValue(word: String): Int = {
    word.split("").map(_.head.toInt - 'A' + 1).reduce(_ + _)
  }

  def isTriangleNumber(n: Int): Boolean = {
    while(maxTriangleNumber < n){
      currentN += 1
      maxTriangleNumber = currentN*(currentN+1)/2
      triangleNumbers += maxTriangleNumber
    }

    triangleNumbers contains n
  }
}
