package adventofcode

import scala.io.Source
import java.nio.file.Paths

trait ProgramWithSource {

  def filename: String

  def fileSource: Iterator[String] = {
    val fileSource = Paths.get(".").toAbsolutePath.resolve(filename)
    Source
      .fromFile(fileSource.toString())
      .getLines()
  }
}
