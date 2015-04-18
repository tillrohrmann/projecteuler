
object Solution51 {
  def main(args: Array[String]): Unit = {
    val familySize = 8
    val stream = Stream.from(2).filter(Helper.isPrime(_))

    val result = stream.map(x => (x, calcFamily(x))).filter(x => x._2 == familySize).head

    println(result)
  }



  /** Number is a prime
    *
    * @param number
    * @return
    */
  def calcFamily(number: Int): Int = {
    val digits = Helper.digits(number)

    val maxFamilySizeOne = if(digits(0) == 1) {
      val ones = findValues(digits, 1).tail.toArray
      0 to ones.length map {
        familyWithReplacementLength(digits, ones, _, true)
      } max
    } else {
      0
    }

    val zeros = findValues(digits, 0).toArray

    if(zeros.nonEmpty) {
      val maxFamilySizeZero = 1 to zeros.length map {
        familyWithReplacementLength(digits, zeros, _)
      } max

      math.max(maxFamilySizeOne, maxFamilySizeZero)
    } else {
      maxFamilySizeOne
    }
  }

  def familyWithReplacementLength(digits: List[Int], positions: Array[Int], length: Int, one: Boolean = false): Int = {
    val selection = new Array[Int](length)
    var maxFamilySize = 0

    for(i <- 0 until length){
      selection(i) = i
    }

    var hasNext = true

    while(hasNext) {
      val size =  familySizeOfSelection(digits, selection, positions, one)

      if(size > maxFamilySize) {
        maxFamilySize = size
      }

      hasNext = nextSelection(selection, length)
    }

    maxFamilySize
  }

  def nextSelection(selection: Array[Int], max: Int): Boolean = {
    var counter = selection.length - 1

    while(counter >= 0 && selection(counter) == max-(selection.length - counter)) {
      counter -= 1
    }

    if(counter < 0){
      false
    } else {
      selection(counter) += 1

      var value = selection(counter) + 1
      counter += 1


      while(counter < selection.length) {
        selection(counter) = value
        value += 1
        counter += 1
      }

      true
    }
  }

  def familySizeOfSelection(digits: List[Int], selection: Array[Int], positions: Array[Int], one: Boolean): Int = {
    val numPrimesInFamily = 1 to 9 map {
      x => {
        val number = replaceDigits(digits, selection, positions, x, one)
        if(Helper.isPrime(number)) 1 else 0
      }
    } reduce ( _ + _ )

    if(one) {
      numPrimesInFamily
    } else {
      numPrimesInFamily + 1
    }
  }

  def replaceDigits(digits: List[Int], selection: Array[Int], positions: Array[Int], value: Int, one: Boolean): Long = {
    val array = digits.toArray

    if(one) {
      array(0) = value
    }

    for(idx <- selection) {
      array(positions(idx)) = value
    }

    array.foldLeft(0l)((result, digit) => result * 10 + digit)
  }

  def findValues(digits: List[Int], value: Int): List[Int] = {
    digits.zipWithIndex.flatMap{ case (digit, idx) => if(digit == value) Some(idx) else None}
  }
}
