package adventofcode

import scala.io.Source
import java.nio.file.Paths

object SumMultiply extends App {

  val fileSource = Paths.get(".").toAbsolutePath.resolve("1-source.txt")
  val arr = Source
    .fromFile(fileSource.toString())
    .getLines
    .toArray
    .map(Integer.parseInt)

  for (i <- 0 until arr.length) {
    for (j <- i until arr.length) {
      for (k <- j until arr.length) {
      if (arr(i) + arr(j) + arr(k) == 2020) {
        println(arr(i) * arr(j) * arr(k))
      }
    }
    }
  }

}
