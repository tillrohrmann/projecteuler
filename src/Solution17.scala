
object Solution17 {

  val singleDigit = Map(
    ("0" -> ""), ("1" -> "one"), ("2" -> "two"), ("3" -> "three"), ("4" -> "four"),
    ("5" -> "five"), ("6" -> "six"), ("7" -> "seven"), ("8" -> "eight"), ("9" -> "nine")
  )

  val doubleDigit = Map(
    ("0" -> ""),
    ("10" -> "ten"), ("11" -> "eleven"), ("12" -> "twelve"), ("13" -> "thirteen"), ("14" ->
      "fourteen"), ("15" -> "fifteen"), ("16" -> "sixteen"), ("17" -> "seventeen"), ("18" ->
      "eighteen"), ("19" -> "nineteen"), ("2" -> "twenty"), ("3" -> "thirty"), ("4" -> "forty"),
    ("5" -> "fifty"), ("6" -> "sixty"), ("7" -> "seventy"), ("8" -> "eighty"), ("9" -> "ninety")
  )
  def main(args: Array[String]): Unit = {
    println(toWord("342"))

    val result = 1 to 1000 map {_.toString} map {toWord(_)} map { x => println(x); x.length } reduce
      {_ + _}

    println(result)
  }

  def toWord(value: String): String = {
    if (value.length == 1) {
      singleDigit(value)
    } else {
      if (value.length == 2) {
        if (value.head == '1') {
          doubleDigit(value)
        } else {
          doubleDigit(value.head.toString) + toWord(value.tail)
        }
      }else{
        if(value.length == 3){
          val tail = toWord(value.tail)
          singleDigit(value.head.toString) + "hundred" + (if(tail.nonEmpty) ("and" + tail) else "")
        }else{
          "onethousand"
        }
      }
    }
  }
}
