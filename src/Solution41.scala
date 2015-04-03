
object Solution41 {
  def main(args: Array[String]): Unit = {
    val pandigitalPrimes = 1 to 9 flatMap(createPandigital(_).filter(Helper.isPrime(_)))
    println(pandigitalPrimes.max)
  }

  def createPandigital(n: Int): List[Int] = {
    val perms = 1 to n permutations

    perms.map{x => x.foldLeft(0)((result, elem) => result*10 + elem)} toList
  }
}
