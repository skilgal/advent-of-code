package adventofcode

import scala.collection.mutable

object Jolts extends App with ProgramWithSource {

  def filename: String = "jolts-source.txt"

  var prev = 0
  var aggr = mutable.Map[Int, Int]()
  for (value <- fileSource.toList.map(el => Integer.parseInt(el)).sorted) {
    val diff = value - prev
    println(s"value ${value} and prev ${prev}. found diff ${diff}")
    val cached = aggr.getOrElse(diff, 0)
    aggr.put(diff, cached + 1)
    prev = value
  }

  println(aggr.toString())

  println(aggr(1) * ( 1 + aggr(3)))
}
