package adventofcode

import javax.swing.text.Position
import scala.annotation.tailrec

object RainRisk extends App with ProgramWithSource {

  // override def filename: String = "test.txt"
  override def filename: String = "rain-risk.txt"

  case class Position(east: Int, north: Int)

  implicit class PositionOps(pos: Position) {
    def +(diff: Position): Position = {
      Position(pos.east + diff.east, pos.north + diff.north)
    }

    def -(endPos: Position): Position = {
      Position(endPos.east - pos.east, endPos.north - pos.north)
    }

    def *(count: Int): Position = {
      Position(pos.east * count, pos.north * count)
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

  def rotate(point: Position, times: Int): Position = times match {
    // Error could be here
    case times if times % 4 == 0 => point
    case times if times % 4 == 1 => Position(point.north, -point.east)
    case times if times % 4 == 2 => Position(-point.east, -point.north)
    case times if times % 4 == 3 => Position(-point.north, point.east)
    case times if times < 0      => rotate(point, 4 + times)
  }

  def time[T](name: String, op: => T): T = {
    val time = System.currentTimeMillis()
    val res = op
    println(s"Operation: ${name} took ${System.currentTimeMillis() - time}")

    res
  }

  def stepTo(direction: WorldSide, steps: Int): Position = direction match {
    case East  => Position(steps, 0)
    case West  => Position(-steps, 0)
    case North => Position(0, steps)
    case South => Position(0, -steps)
  }

  var facingSide: WorldSide = WorldSide.East
  var shipPosition = Position(0, 0)
  var wayPoint = Position(10, 1)

  def move(line: String): Unit = {
    val direction = line.head
    val stepCount = Integer.parseInt(line.tail)

    direction match {
      case 'N' => wayPoint += stepTo(North, stepCount)
      case 'S' => wayPoint += stepTo(South, stepCount)
      case 'E' => wayPoint += stepTo(East, stepCount)
      case 'W' => wayPoint += stepTo(West, stepCount)
      case 'L' => wayPoint = rotate(wayPoint, -stepCount / 90)
      case 'R' => wayPoint = rotate(wayPoint, stepCount / 90)
      case 'F' =>
        for (_ <- 1 to stepCount) {
          shipPosition += wayPoint
        }
    }

    // println(s"command: ${line}")
    // println(s"Ship position: ${shipPosition}")
    // println(s"Waypoint ${wayPoint}")
  }

  fileSource.toList.map(move)

  println(
    s"Final position is ${shipPosition.east} + ${shipPosition.north} = ${Math
      .abs(shipPosition.east) + Math.abs(shipPosition.north)}"
  )

}
