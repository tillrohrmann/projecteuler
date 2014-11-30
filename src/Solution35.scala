import scala.collection.mutable

object Solution35 {
  val primes = collection.mutable.Set[Int]()

  def main(args: Array[String]): Unit = {

    val l = 2 until 1000000 filter (isCircularPrime(_))

    println(l.length)

  }

  def isCircularPrime(n: Int): Boolean = {
    val digits = n.toString.split("").map(_.toInt)
    val q = mutable.Queue(digits:_*)

    (1 to digits.length) forall {
      _ => {


       val result = isPrime(q.mkString("").toInt)

        val head = q.dequeue()
        q.enqueue(head)

        result
      }
    }
  }

  def isPrime(n: Int): Boolean = {
    if(primes.contains(n)){
      true
    }else{
      val max = math.floor(math.sqrt(n)).toInt

      if((2 to max) forall  (n % _ != 0)){
        primes += n
        true
      }else{
        false
      }
    }
  }
}
