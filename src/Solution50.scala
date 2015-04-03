
object Solution50 {
  def main(args: Array[String]): Unit = {
    val max = 1000000
    val primes = (2 until max).filter(Helper.isPrime(_))

    val result = (0 until primes.length).par.flatMap{
      start => {
        var sum = 0
        var index = start
        var solutionSet: Set[(Int, Int)] = Set()

        while(sum < max && index < primes.length) {
          sum += primes(index)

          if(Helper.isPrime(sum)){
            solutionSet += (index - start + 1 )-> sum
          }

          index += 1
        }

        solutionSet
      }.seq
    }

    println(result.maxBy(_._1))

    // (543,997651)
  }


}
