
object Solution39 {
  def main(args: Array[String]): Unit = {
    val solutions = 1 to 1000 map (x => (numberSolutions(x), x))

    println(solutions.maxBy(_._1))
  }

  def numberSolutions(p: Int): Int = {
    1 to (p/4) filter {
      b =>
        (2*b*p -p*p) % (2*(b-p)) == 0
    } length
  }
}
