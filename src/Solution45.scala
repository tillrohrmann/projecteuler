
object Solution45 {

  def main(args: Array[String]): Unit = {
    val result = findTriangleNumber((286, 166, 144))

    println(result + " " + t(result._1))
  }

  def findTriangleNumber(start: (Long, Long, Long)): (Long, Long, Long) = {
    var (nt, np, nh) = start
    var (vt, vp, vh) = (t(nt), p(np), h(nh))

    while(vt != vp || vt != vh || vp != vh) {
      if(vt < vp || vt < vh) {
        nt += 1
        vt = t(nt)
      } else if(vp < vh) {
        np += 1
        vp = p(np)
      } else {
        nh += 1
        vh = h(nh)
      }
    }

    (nt, np, nh)
  }

  def t(n: Long): Long = {
    n*(n + 1)/2
  }

  def p(n: Long): Long = {
    n*(3*n - 1)/2
  }

  def h(n: Long): Long = {
    n*(2*n - 1)
  }
}
