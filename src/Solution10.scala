object Solution10{
	def main(args: Array[String]): Unit = {
		val primes = collection.mutable.HashSet[Int]()
		val maxNumber = 2000000
		val seq = 2 to maxNumber
		seq.foldLeft(0L){
			(sum, elem) =>
				if(primes.forall{ elem % _ != 0}){
					primes.add(elem)
					sum+elem
				}else{
					sum
				}
		}

	}
}
