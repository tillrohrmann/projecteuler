
object Solution18 {
  def main(args: Array[String]): Unit = {
    val input = """75
    |95 64
    |17 47 82
    |18 35 87 10
    |20 04 82 47 65
    |19 01 23 75 03 34
    |88 02 77 73 07 63 67
    |99 65 04 28 06 16 70 92
    |41 41 26 56 83 40 80 70 33
    |41 48 72 33 47 32 37 16 94 29
    |53 71 44 65 25 43 91 52 97 51 14
    |70 11 33 28 77 73 17 78 39 68 17 57
    |91 71 52 38 17 14 91 43 58 50 27 29 48
    |63 66 04 68 89 53 67 30 73 16 69 87 40 31
    |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin

    val lines = input.split("\n")

    val data = lines map { _.split(" ") map {_.toInt}}

    val maxPath = new Array[Array[Int]](data.length)

    for(i <- 0 until data.length-1){
      maxPath(i) = new Array[Int](i+1)
    }

    maxPath(data.length-1) = data(data.length-1)

    for(row <- data.length-2 to 0 by -1){
      solveRow(maxPath, data, row)
    }
    printArray(maxPath)
  }

  def solveRow(path:Array[Array[Int]], data:Array[Array[Int]], row: Int): Unit = {
    for(i <- 0 until path(row).length){
      val value = data(row)(i)
      path(row)(i) = Math.max(path(row+1)(i) + value, path(row+1)(i+1) + value)
    }
  }

  def printArray(data: Array[Array[Int]]): Unit = {
    val s = data map { line =>
      line.mkString(" ")
    } mkString("\n")

    println(s)
  }
}
