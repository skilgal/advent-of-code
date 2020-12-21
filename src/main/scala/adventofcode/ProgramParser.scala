package adventofcode

import java.nio.file.Paths
import scala.io.Source

object ProgramParser extends App {

  val fileSource = Paths.get(".").toAbsolutePath.resolve("program-source.txt")
  val data = Source
    .fromFile(fileSource.toString())
    .getLines()
    .toArray

  var usedIndexes = scala.collection.mutable.ListBuffer[Int]()

  val nopC = """nop\s([-+]\d+)""".r
  val jmpC = """jmp\s([-+]\d+)""".r
  val accC = """acc\s([-+]\d+)""".r

  def parse(accum: Int, index: Int, changed: Boolean): Boolean = {
    if (usedIndexes.contains(index)) {
      false
    } else if (index >= data.length) {
      true
    } else {
      usedIndexes += index
      data(index) match {
        case nopC(count) =>
          println(s"index is ${index};\t ${data(index)}")
          val res = parse(accum, index + 1, changed)
          if (!res && !changed) {
            parse(accum, index + Integer.parseInt(count), true)
          } else res
        case jmpC(count) =>
          println(s"index is ${index};\t ${data(index)}")
          val res = parse(accum, index + Integer.parseInt(count), changed)
          if (!res && !changed) {
            parse(accum, index + 1, true)
          } else res
        case accC(count) =>
          parse(accum + Integer.parseInt(count), index + 1, changed)
      }
    }
  }

  parse(0, 0, false)
}
