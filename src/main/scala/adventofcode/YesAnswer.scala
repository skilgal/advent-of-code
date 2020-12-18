package adventofcode

import scala.io.Source
import java.nio.file.Paths

object YesAnswer extends App {

  val fileSource = Paths.get(".").toAbsolutePath.resolve("yes-source.txt")
  val data = Source
    .fromFile(fileSource.toString())
    .mkString

  val yesAnswers = data
    .split("\n\n")
    .map { groupped =>
      val res = groupped
        .split("\n")
        .toList
        .reduce { (agg: String, el: String) => agg.intersect(el) }
      res.length()
    }
    .sum

  println(yesAnswers)
}
