package adventofcode

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object Xmas extends App with ProgramWithSource {

  override def filename: String = "xmas-source.txt"
  val preambleSize = 25

  // var preamble: Queue[Int] = Queue.from(
  //   fileSource.toList.take(preambleSize).map(str => Integer.parseInt(str))
  // )

  // def findSumNums(num: Int): Boolean = {
  //   for (i <- 0 until preamble.size) {
  //     for (j <- i until preamble.size) {
  //       if (preamble(i) + preamble(j) == num) {
  //         return true
  //       }
  //     }
  //   }
  //   false
  // }

  // fileSource
  //   .drop(preambleSize)
  //   .map { line =>
  //     val number = Integer.parseInt(line)
  //     if (findSumNums(number)) {} else {
  //       println(s"For number ${number} can't find the sum number")
  //       System.exit(0)
  //     }

  //     preamble.dequeue()
  //     preamble.enqueue(number)
  //   }
  //   .toList

  val num = 400480901
  // val num = 127

  val source = fileSource
    .takeWhile(_.trim() != num.toString())
    .map(el => Integer.parseInt(el))
    .toList

  println(s"source size: ${source.size}")

  for (frame <- 2 to source.size) {
    println(s"choose frame as ${frame}")
    val queue = Queue.from[Int](source.take(frame))
    for (elem <- source.drop(frame)) {
      if (queue.sum == num) {
        println(
          s"Found the sequence. Size is ${frame}, queue: ${queue.mkString(",")}"
        )
        println(s"needed sum ${queue.min + queue.max}")
        System.exit(0)
      }
      queue.dequeue()
      queue.enqueue(elem)
    }
  }

}
