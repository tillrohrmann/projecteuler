import java.math.BigInteger

object Solution16 {
  def main(args: Array[String]): Unit = {
    val a = new BigInt(BigInteger.valueOf(1))
    val b = a.<<(1000)

    val digits = b.toString().split("")

    val result = digits map {_.toInt } reduce { _ + _}

    println(result)
  }
}
