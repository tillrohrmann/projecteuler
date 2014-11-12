import scala.io.Source
import scala.util.Sorting

object Solution22 {
  def main(args: Array[String]): Unit = {
    val line = Source.fromFile("p022_names.txt").getLines().next()
    val names = line.split(",").map{ name => name.substring(1, name.length-1)}

    Sorting.quickSort(names)

    val partialScores = names map {
      name =>{
        val chrs = name.split("")

        val r = chrs.map{ _.charAt(0).toInt - 'A'.toInt + 1}

        r.reduce(_ + _)
      }
    }

    val scores = partialScores.zipWithIndex.map {
      case (partialScore, idx) => partialScore * (idx + 1).toLong
    }

    println(names(937))
    println(partialScores(937))
    println(scores(937))

//    println(partialScores.mkString("\n"))
//    println(scores.length)
//    println(scores.mkString("\n"))
    println(scores reduce (_ + _))

  }
}
