
object Solution24 {
  def main(args: Array[String]): Unit = {
    val list = 0 to 9
    val permutations = list.permutations.toList

    println(permutations(999999))
  }
}
