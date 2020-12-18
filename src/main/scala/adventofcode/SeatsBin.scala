package adventofcode

import scala.io.Source
import java.nio.file.Paths

object SeatsBin extends App {

  def seat(num: Int): String = {
    val col = num / 8
    val row = num % 8
    val res = Integer
      .toBinaryString(col)
      .reverse
      .padTo(7, 'F')
      .reverse
      .replaceAll("0", "F")
      .replaceAll("1", "B") ++
      Integer
        .toBinaryString(row)
        .reverse
        .padTo(3, 'L')
        .reverse
        .replaceAll("0", "L")
        .replaceAll("1", "R")
    res
  }

  def id(seat: String): Int = {
    Integer.parseInt(
      seat.take(7).replaceAll("F", "0").replaceAll("B", "1"),
      2
    ) * 8 + Integer.parseInt(
      seat.takeRight(3).replaceAll("L", "0").replaceAll("R", "1"),
      2
    )
  }

  val fileSource = Paths.get(".").toAbsolutePath.resolve("seat-source.txt")
  val data = Source
    .fromFile(fileSource.toString())
    .getLines().toList

  println(s"min is ${data.map(id).min}")
  println(s"max is ${data.map(id).max}")

  for (i <- 7 until 894) {
    if (
      !data.contains(seat(i)) &&
      data.contains(seat(i + 1)) && data.contains(seat(i - 1))
    ) {
      println(i)
      println(seat(i))
    }
  }
}
