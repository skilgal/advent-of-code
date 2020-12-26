package adventofcode
import util.control.Breaks._

object SeatsFilling extends App with ProgramWithSource {

  override def filename: String = "seats-source.txt"

  // override def filename: String = "seats-source.txt"

  case class Pos(line: Int, row: Int)

  def inPlace(pos: Pos, place: Array[String]): Boolean = {
    pos.line >= 0 && pos.row >= 0 &&
    pos.line < place.length &&
    pos.row < place(pos.line).length
  }

  def firstSeatInDirection(
      seat: Pos,
      place: Array[String],
      next: Pos => Pos
  ): Option[Char] = {
    var firstSeen = next(seat)

    while (inPlace(firstSeen, place) && place(firstSeen.line)(firstSeen.row) == '.') {
      firstSeen = next(firstSeen)
    }

    if (inPlace(firstSeen, place)) {
      Some(place(firstSeen.line)(firstSeen.row))
    } else None
  }

  // println(firstSeatInDirection(Pos(1, 1), Array("...", "###", "..."), p => Pos(p.line - 1, p.row - 1)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line - 1, p.row)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line - 1, p.row + 1)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line, p.row - 1)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line + 1, p.row + 1)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line + 1, p.row)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line + 1, p.row - 1)))
  // println(firstSeatInDirection(Pos(1,1), Array("...","###","..."), p => Pos(p.line, p.row - 1)))

  // System.exit(1)

  def findAdjustments(seat: Pos, place: Array[String]): String = {

    val res = List(
      firstSeatInDirection(seat, place, p => Pos(p.line - 1, p.row - 1)),
      firstSeatInDirection(seat, place, p => Pos(p.line - 1, p.row)),
      firstSeatInDirection(seat, place, p => Pos(p.line - 1, p.row + 1)),
      firstSeatInDirection(seat, place, p => Pos(p.line, p.row - 1)),
      firstSeatInDirection(seat, place, p => Pos(p.line, p.row + 1)),
      firstSeatInDirection(seat, place, p => Pos(p.line + 1, p.row - 1)),
      firstSeatInDirection(seat, place, p => Pos(p.line + 1, p.row)),
      firstSeatInDirection(seat, place, p => Pos(p.line + 1, p.row + 1)),
    ).flatten.mkString

    res
  }

  def applyRules(seatPos: Pos, place: Array[String]): Char = {
    val seat = place(seatPos.line)(seatPos.row)
    val neighborns: String = findAdjustments(seatPos, place)
    if (seat == 'L' && neighborns.forall(_ == 'L')) '#'
    else if (seat == '#' && neighborns.filter(_ == '#').size >= 5) 'L'
    else seat
  }

  def change(place: Array[String]): Array[String] = {
    (for (line <- 0 until place.length) yield {
      (for (seat <- 0 until place(line).length) yield {
        applyRules(Pos(line, seat), place)
      }).mkString
    }).toArray
  }

  val seats = fileSource.toArray
  seats.foreach(println)

  var prev = seats
  var gened = prev

  while (gened.sameElements(prev)) {
    gened = change(prev)

    if (gened.sameElements(prev)) {
      println(s"on the step prev == gened")

      println(prev.mkString.filter(_ == '#').size)
      System.exit(0)
    } else {
      prev = gened
    }
  }
}
