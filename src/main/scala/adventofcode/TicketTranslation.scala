package adventofcode

import scala.collection.mutable

object TicketTranslation extends App with ProgramWithSource {
  //  override def filename = "test.txt"
  override def filename = "day16.txt"

  case class Requirement(name: String, low: Range, high: Range) {
    def correct(num: Int): Boolean =
      low.contains(num) || high.contains(num)
  }

  val requirementTemplate = """([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  val requirements = fileSource.takeWhile(_.trim.nonEmpty)
    .toList
//    .map { line =>
//      println(s"read ${line}")
//      line
//    }
    .map {
      case requirementTemplate(name, fFrom, fTo, sFrom, sTo) =>
        Requirement(name,
          Integer.parseInt(fFrom) to Integer.parseInt(fTo),
          Integer.parseInt(sFrom) to Integer.parseInt(sTo))
    }

  //  val nearByTicketsFailedSum = fileSource.dropWhile(_.trim != "nearby tickets:").drop(1).toList.map { line =>
  //    val nums = line.split(",").map(Integer.parseInt)
  //    val (correct, incorrect) = nums.partition(num => requirements.exists(_.correct(num)))
  //    incorrect.sum
  //  }.sum
  //  println(nearByTicketsFailedSum)

  val nearByCorrectTickets = fileSource.dropWhile(_.trim != "nearby tickets:").drop(1).toList.map { line =>
    line.split(",").map(Integer.parseInt)
  }.filter{ numbers =>
    numbers.forall(num => requirements.exists(_.correct(num)))
  }

  println(nearByCorrectTickets.size)
  var correctFields = mutable.Map[Int, Set[String]]()

  for (fieldCount <- nearByCorrectTickets.head.indices) {
    requirements.foreach { requirement =>
      if (nearByCorrectTickets.map(numbers => {
//        println(s"will try to find in array ${numbers.mkString(",")} number no position ${fieldCount}")
        numbers(fieldCount)
      }).forall(requirement.correct)) {
        val oldVal = correctFields.getOrElse(fieldCount, Set(requirement.name))
        correctFields.update(fieldCount, oldVal + requirement.name)
      }
    }
  }

  println(s"correct fields ${correctFields}")

  var foundFields = correctFields.filter(_._2.size == 1)
  println(s"found corret fields $foundFields")

  for (_ <- 0 to 20) {
    foundFields = foundFields ++ correctFields.filter(_._2.size == 1)
    val correctRequs = foundFields.values.flatten
    correctFields = correctFields.filter(field => !foundFields.keySet.contains(field._1))
      .map { case (key, reqNames) =>
        (key, reqNames -- correctRequs)
      }
  }
  println(s"final foundFields is $foundFields")

  val neededKeys: Iterable[Int] = foundFields.map { case (index, fields) => index -> fields.head }
    .filter(_._2.startsWith("departure")).keys.toArray

  val neededValues: Array[Long] = "191,139,59,79,149,83,67,73,167,181,173,61,53,137,71,163,179,193,107,197".split(",").map(Integer.parseInt).map(_.toLong)

  println(neededKeys.map(index => neededValues(index)))
  println(neededKeys.map(index => neededValues(index)).product)

}
