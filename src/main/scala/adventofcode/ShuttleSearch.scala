package adventofcode

import scala.util.control.Breaks._
object ShuttleSearch extends App with ProgramWithSource {
  // override def filename: String = "shuffle-search.txt"

  override def filename: String = "test.txt"

  val time = Integer.parseInt(fileSource.next())
  // val busses = fileSource
  //   .drop(1)
  //   .next()
  //   .split(',')
  //   .filterNot(_ == "x")
  //   .map(Integer.parseInt)

  // println(time)
  // println(busses.mkString(","))

  // var current: Integer = time
  // var busNumber = 0
  // breakable {
  //   while (true) {
  //     val needed = busses.filter(bus => current % bus == 0)
  //     if (needed.nonEmpty) {
  //       println(s"Found first needed bus, it's ${needed.head} in time ${current}")
  //       println(needed.head)
  //       busNumber = needed.head
  //       break()
  //     }
  //     current += 1
  //   }
  // }

  // println(s"You need to wait ${current - time} and the result num is ${(current -time) * busNumber}")
  //
  def stime[T](name: String, op: => T): T = {
    val time = System.currentTimeMillis()
    val res = op
    println(s"Operation: ${name} took ${System.currentTimeMillis() - time}")

    res
  }

  def contest(busses: Array[String]): Int = {
    val requirements = busses.zipWithIndex
      .collect {
        case (value, index) if value != "x" =>
          (Integer.parseInt(value), index)
      }

    var time = 0
    while (
      !requirements.forall { case (bus, timeDiff) =>
        (time + timeDiff) % bus == 0
      }
    ) {
      time += requirements.head._1
    }
    time
  }

  val a = contest(fileSource.drop(1).next().split(","))
  println(s"found the num ${a}")

  println(contest(Array("67", "7", "59", "61")) == 754018)
  println(contest(Array("67", "x", "7", "59", "61")) == 779210)
  println(contest(Array("67", "7", "x", "59", "61")) == 1261476)
  println(contest(Array("1789", "37", "47", "1889")) == 1202161486)
  println(contest(Array("7", "13", "x", "x", "59", "x", "31", "19")) == 1068781)

}
