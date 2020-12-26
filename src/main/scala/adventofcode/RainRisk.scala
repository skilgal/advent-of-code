package adventofcode

object RainRisk extends App with ProgramWithSource {

  override def filename: String = "rain-risk.txt"

  case class Position(east: Int, south: Int)
  implicit class Addicter(pos: Position) {
    def +(diff: Position): Position = {
      Position(pos.east + diff.east, pos.south + diff.south)
    }
  }

  trait WorldSide {
    def toString: String
  }
  object WorldSide {
    object East extends WorldSide {
      override def toString() = "East"
    }
    object West extends WorldSide {
      override def toString(): String = "West"
    }
    object North extends WorldSide {
      override def toString(): String = "North"
    }
    object South extends WorldSide {
      override def toString(): String = "South"
    }
  }
  import WorldSide._

  val stepPattern = """(\w)(\d+)""".r

  def rotate(facing: WorldSide, times: Int): WorldSide = {
    if (times == 0) facing
    else
      facing match {
        case WorldSide.East =>
          if (times > 0) rotate(South, times - 1)
          else rotate(North, times + 1)
        case West =>
          if (times > 0) rotate(North, times - 1)
          else rotate(South, times + 1)
        case North =>
          if (times > 0) rotate(East, times - 1)
          else rotate(West, times + 1)
        case South =>
          if (times > 0) rotate(West, times - 1)
          else rotate(East, times + 1)
      }
  }

  def stepTo(direction: WorldSide, steps: Int): Position = direction match {
    case East  => Position(steps, 0)
    case West  => Position(-steps, 0)
    case North => Position(0, -steps)
    case South => Position(0, steps)
  }

  var facingSide: WorldSide = WorldSide.East
  var position = Position(10, -1)

  // move("N1")
  // move("S1")
  // move("E1")
  // move("W1")
  // move("F1")
  // move("R180")
  // move("F2")
  // move("L180")
  // move("F1")
  // println(position)

  // System.exit(0)

  def move: PartialFunction[String, Unit] = { case stepPattern(direction, stepCountStr) =>
    val stepCount = Integer.parseInt(stepCountStr)

    direction match {
      case "N" => position += stepTo(North, stepCount)
      case "S" => position += stepTo(South, stepCount)
      case "E" => position += stepTo(East, stepCount)
      case "W" => position += stepTo(West, stepCount)
      case "L" => facingSide = rotate(facingSide, -stepCount / 90)
      case "R" => facingSide = rotate(facingSide, stepCount / 90)
      case "F" => position += stepTo(facingSide, stepCount)
    }
  }

  fileSource.toList.map(move)

  println(
    s"Final position is ${position.east} + ${position.south} = ${Math.abs(position.east) + Math.abs(position.south)}"
  )

}
