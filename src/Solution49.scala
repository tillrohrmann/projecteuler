
object Solution49 {
  def main(args: Array[String]): Unit = {
    val primes = (1000 to 9999).filter(Helper.isPrime(_))

    val groups = primes.groupBy(p => Helper.digits(p).sorted)

    println(groups.filter(p => checkCondition(p._2)))

    //296962999629
  }

  def checkCondition(values: IndexedSeq[Int]): Boolean = {
    val set = values.toSet

    values.exists(n => set.contains(n+3330) && set.contains(n + 6660))
  }
}
