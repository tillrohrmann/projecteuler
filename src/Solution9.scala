object Solution9{
  def main(args: Array[String]): Unit = {
    val triplets = for(c <- 0 to 1000; b <- 0 to 1000-c) yield (c, b, 1000-c-b)
    val triplet = triplets filter { case (c,b,a) => c*c == b*b + a*a }
    val (c,b,a) = triplet(0)
    c*b*a
  }
}