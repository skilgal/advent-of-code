package adventofcode

import java.nio.file.Paths
import scala.io.Source

object BagsContainer extends App with ProgramWithSource {

  case class Bag(count: Int, name: String)

  override val filename = "bags-source.txt"

  val descriptionR = """(.*) bags contain (.*)""".r
  val noBags = "no other bags.".r
  val bagListR = """(.*), (.*)""".r
  val bagR = """(\d+) (\w+\s\w+) bag.*""".r

  def parse(descr: String): Map[String, List[Bag]] = descr match {
    case descriptionR(container, bagList) =>
      Map(container -> parseBagList(bagList))
    case _ =>
      throw new IllegalArgumentException(s"can't parse description ${descr}")
  }

  def parseBagList(list: String): List[Bag] = {
    list match {
      case noBags() => List.empty[Bag]
      case bagListR(f, nexts) =>
        parseBagList(f.trim) ++ parseBagList(nexts.trim)
      case bagR(count, name) => List(Bag(Integer.parseInt(count), name))
      case _ =>
        throw new IllegalArgumentException(s"can't parse bag list '${list}'")
    }
  }

  val relation: Map[String, List[Bag]] = fileSource.toList
    .map { line =>
      parse(line)
    }
    .reduce((m1, m2) => m1 ++ m2)

  def findOwner(relation: Map[String, List[Bag]], bag: String): Set[String] = {
    relation.filter { p =>
      p._2.map(_.name).contains(bag)
    }.keySet
  }

  def findChildrenCount(relation: Map[String, List[Bag]], bag: String): Int = {
    val children = relation
      .filter(p => p._1 == bag)
      .values.flatten
    children.size

    children.map {child =>
      child.count + child.count * findChildrenCount(relation, child.name)
    }.sum
  }

  var allOwners = scala.collection.mutable.Set[String]()
  var owners = findOwner(relation, "shiny gold")
  allOwners ++= owners
  while (owners.nonEmpty) {
    owners = owners.flatMap(bag => findOwner(relation, bag))
    allOwners ++= owners
  }

  println(allOwners.size)

  println(findChildrenCount(relation, "shiny gold"))

}
