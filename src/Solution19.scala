
object Solution19 {
  def main(args: Array[String]): Unit = {
    var currentDay = 1
    val list = for(year <- 1900 until 2000; month <- 1 to 12) yield {
      currentDay = (currentDay + getNumberOfDays(month, year)) % 7
      currentDay
    }

    val days = 1 :: list.toList

    println(days.filter(_ == 0).length)
  }

  def getNumberOfDays(month: Int, year: Int): Int = {
    if(year == 2)
      if(isLeapYear(year)) 29 else 28
    else{
      if(year == 4 || year == 6 || year == 9 || year == 11){
        30
      }else {
        31
      }
    }
  }

  def isLeapYear(year: Int): Boolean = {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
  }
}
