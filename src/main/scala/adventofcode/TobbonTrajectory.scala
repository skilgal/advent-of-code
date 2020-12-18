package adventofcode

import scala.io.Source
import java.nio.file.Paths

object TobbonTrajectory extends App {

  def skip[A](l: List[A], n: Int) =
    l.zipWithIndex.collect { case (e, i) if ((i + 1) % n) == 0 => e }

  val fileSource =
    Paths.get(".").toAbsolutePath.resolve("trajectory-source.txt")

  var column = 0
  var row    = 0

  val data = Source
    .fromFile(fileSource.toString())
    .getLines.toArray

  var counter = 0

  for (j <- 2 until data.length by 2) {
    val line = data(j)
    // println(s"line $j is ${line}")
    column += 1
    // println(s"column is ${column}")
    val treeLine = if (column >= line.length) {
      // println(s"column is ${column}")
      val repeatCount = column / line.length + 1
      val genLine     = line * repeatCount
      genLine(column)
    } else line(column)

    // println(s"result field is ${treeLine}")

    if (treeLine == '#')
      counter += 1
  }

  println(counter)

}
