package adventofcode

/*

    Find min number of workers required to execute all tasks without latency

    Task is described as (startTime, duration, workersRequired)

    Example:

    (1, 2, 4)
    (2, 3, 2)
    (1, 2, 1)

    |_44__
    |__222
    |_11__

    4 + 2 + 1 = 7 workers required

 */

object WorkersAtTime {

  private val input = Array(
    Array(1, 2, 4),
    Array(2, 3, 2),
    Array(1, 2, 1)
  )

  def main(args: Array[String]): Unit = {
    val res = input.flatMap { arrOf3 =>
      (arrOf3(0) to arrOf3(1))
        .map(_ -> arrOf3(2))
    }
      .groupBy(_._1)
      .values
      .map(_.map(_._2).sum).max

    println(res)
  }

}
