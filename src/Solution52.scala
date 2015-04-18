
object Solution52 {
  def main(args: Array[String]): Unit = {
    val stream = Stream.from(1).filter{
      x => {
        val digits = createDigits(x)
        containsSameElements(digits)
      }
    }

    println(stream.head)

//    val number = 125874
//    val digits = createDigits(number)
//    println(digits)
//    println(containsSameElements(digits))
  }

  def containsSameElements(digits: List[List[Int]]): Boolean = {
    val head = digits.head.sorted

    digits.tail.forall(x => head.sameElements(x.sorted))
  }

  def createDigits(number: Long): List[List[Int]] = {
    val result = 1 to 6 map {
      number * _
    } map {
      Helper.digits(_)
    }

    result.toList
  }
}
