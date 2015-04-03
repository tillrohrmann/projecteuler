
object Solution48 {
  def main(args: Array[String]): Unit = {

    val result = (1 to 1000).map{x => BigInt(x).pow(x)}.reduce(_ + _)

    println(result)
  }
}
