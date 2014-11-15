
object Solution32 {
  def main(args: Array[String]): Unit = {
    val max = 9999
    val products = for(a <- 1 to max; b <- 1 to max if(isPandigital(a,b))) yield {
      a*b
    }

    println(products.toSet.reduce(_ + _))

  }

  def isPandigital(a: Int, b: Int): Boolean = {
    val c = a*b

    val concatenated = a.toString + b.toString + c.toString
    val digits = concatenated.split("").map(_.toInt)

    val occurences = Array.fill(10)(0)

    digits foreach {
      occurences(_) += 1
    }

    occurences(0) == 0 && occurences.tail.forall(_ == 1)
  }
}
