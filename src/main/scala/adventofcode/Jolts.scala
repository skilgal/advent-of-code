package adventofcode

import scala.collection.mutable
import scala.collection.immutable.TreeSet
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import scala.collection.mutable._

object Jolts extends App with ProgramWithSource {

  def filename: String = "jolts-source.txt"
  // def filename: String = "test.txt"

  // var prev = 0
  // var aggr = mutable.Map[Int, Int]()
  // for (value <- fileSource.toList.map(el => Integer.parseInt(el)).sorted) {
  //   val diff = value - prev
  //   println(s"value ${value} and prev ${prev}. found diff ${diff}")
  //   val cached = aggr.getOrElse(diff, 0)
  //   aggr.put(diff, cached + 1)
  //   prev = value
  // }

  // println(aggr.toString())

  // println(aggr(1) * ( 1 + aggr(3)))

  val data = fileSource.toList.map(Integer.parseInt(_))
  val fullData = (0 +: data :+ (data.max + 3)).sorted

  var ways = Map[Int, Long]().withDefaultValue(0)
  ways(0) = 1

  for (i <- 1 until fullData.length) {
    val value = fullData(i)
    // println(s"value is ${value}")
    ways(value) = ways(value - 1) + ways(value - 2) + ways(value - 3)
    // println(s"value path is ${ways(value)}")
  }

  println(s"max is ${fullData.max}")
  println(ways)

}
