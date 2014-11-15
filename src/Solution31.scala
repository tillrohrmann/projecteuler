
object Solution31 {
  def main(args: Array[String]): Unit = {
    println(combinations(200, List(1,2,5,10,20,50,100,200)))
  }

  def combinations(n: Int, coins: List[Int]): Int = {
    coins match {
      case coin::t =>
        val c = (0 to ((n.toDouble/coin).toInt)).toList map {
          x =>
            combinations(n - coin * x, t)
        }

        c.fold(0)({ _ + _ })
      case Nil => if(n == 0) 1 else 0
    }
  }
}
