
object Solution25 {
  def main(args: Array[String]): Unit = {
    var prevprev = BigInt(1)
    var prev = BigInt(1)
    var current = BigInt(1)
    var counter = 2

    while(current.toString.length < 1000){
      prevprev = prev
      prev = current
      current = nextFibonacci(prev, prevprev)
      counter += 1
    }

    println(current)
    println(counter)
  }

  def nextFibonacci(prev: BigInt, prevprev: BigInt): BigInt = {
    prev + prevprev
  }
}
