
object Solution46 {

  def main(args: Array[String]): Unit = {
    val stream = Stream.from(1, 2)

    val result = stream.filter(x => !Helper.isPrime(x) && !composable(x)).head

    println(result)
  }

  def composable(value: Int): Boolean = {
    val stream = Stream.range(1, math.ceil(math.sqrt(value/2.0)).toInt, 1)

    stream.map{
      x => value - 2*x*x
    }.filter(Helper.isPrime(_)).headOption.isDefined
  }

}
