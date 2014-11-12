object Solution14{
	val lengths = scala.collection.mutable.HashMap[Long, Long](1L -> 1L)

	def main(args: Array[String]): Unit = {
		val stream = Stream.from(1)
		val inputs = stream.takeWhile(_ < 1000000)

		inputs foreach {
			x =>
				println(x)
				collatz(List(x))
		}

		println(lengths.max(new Ordering[(Long, Long)] {
			override def compare(x: (Long, Long), y: (Long, Long)): Int = if(x._2 < y._2) -1 else if(x
				._2 > y._2) 1 else 0
		}))
	}

	def collatz(trace: List[Long]): (Long, Long) = {
		val head = trace.head
		require(head > 0)

		if(lengths.contains(head)){
			val length = lengths(head)
			trace.zipWithIndex.foreach{
				case (element, idx) => lengths += (element -> (idx + length))
			}
			(trace.last, lengths(trace.last))
		}else{
			if(head % 2 == 0){
				collatz((head /2)::trace)
			}else{
				collatz((3*head+1)::trace)
			}
		}

	}
}