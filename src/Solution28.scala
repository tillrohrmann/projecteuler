
object Solution28 {
  def main(args: Array[String]): Unit = {
    println(calcSquare(1001))
  }

  def calcSquare(number: Int): Int = {
    if(number % 2 == 0 || number <= 0){
      -1
    }else{
      val t = 3 to number by 2 map { x => 4*x*x - 6*(x-1)}

      t :+ 1 reduce { _ + _ }
    }
  }
}
