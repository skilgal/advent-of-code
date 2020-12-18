package adventofcode

import java.nio.file.Paths
import scala.io.Source

object ValidPass extends App {

  case class PassValidation(min: Int, max: Int, symb: Char, password: String)

  val parser     = """(\d+)-(\d+)\s+([a-z])\:\s+(\w+)""".r
  val fileSource = Paths.get(".").toAbsolutePath.resolve("validpass-source.txt")
  val validPasswords = Source
    .fromFile(fileSource.toString())
    .getLines
    .map {
      case parser(min, max, symb, password) =>
        PassValidation(
          Integer.parseInt(min),
          Integer.parseInt(max),
          symb.head,
          password
        )

      case _ => throw new IllegalArgumentException("hz")

    }
    .toArray
    .filter { validation =>
      if (validation.password(validation.min - 1) == validation.symb) {
        validation.password(validation.max - 1) != validation.symb
      } else {
        validation.password(validation.max - 1) == validation.symb
      }
    }
    .size

  println(validPasswords)

}
