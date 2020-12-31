package adventofcode

import scala.collection.mutable

object RambunctiousRecitation extends App with ProgramWithSource {
  // override def filename = "test.txt"
  override def filename = "day15.txt"

  def solve(needed: Int, data: Array[Int]): Int = {
    var pDict: mutable.Map[Int, Int] = mutable.Map.from(data.zipWithIndex.map {
      case (value, index) =>
        (value -> (index + 1))
    }.toMap)

    var turn = pDict.size + 1
    var same = 0

    while (turn < needed) {
      var diff = 0
      if (pDict.contains(same)) {
        diff = turn - pDict(same)
        pDict(same) = turn
      } else {
        diff = 0
        pDict(same) = turn
      }

      same = diff
      turn += 1
    }

    same
  }


  val data = fileSource.toArray.flatMap(_.split(",")).map(Integer.parseInt)

  println(s"solve for 2020th ${solve(2020, data)}")
  println(s"solve for 3000 ${solve(30000000, data)}")
}
