
object Solution26 {
  def main(args: Array[String]): Unit = {
    val m = 1 until 1000 map {x => (cycle(1, x, List()), x)} maxBy(_._1)

    println(m)
  }

  def cycle(current: Int, denominator: Int, path: List[Int]): Int = {
    if(path.indexOf(current) >= 0){
      path.indexOf(current) + 1
    }else{
      if(current % denominator == 0){
        0
      }else{
        cycle(current*10 % denominator, denominator, current :: path)
      }
    }
  }
}
