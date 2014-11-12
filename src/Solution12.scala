object Solution12{
	def main(args: Array[String]): Unit = {
		def triangularNumber(n: Int): Int = {
			(1 to n).sum
		}

		def numFactors(number: Int): Int = {
			(for(i <- 1 to number/2 if number % i ==0 ) yield 1).sum + 1
		}

		val input = Stream.from(1)

		input.map{ x=> numFactors(triangularNumber(x)) }.dropWhile( _ <= 500 )
	}
}