package adventofcode
import adventofcode.OperationOrder.Operation.{Sum, Multiply, Num}
import scala.util.parsing.combinator.RegexParsers

object OperationOrder extends App with ProgramWithSource with RegexParsers {

  // override def filename: String = "test.txt"
  override def filename: String = "operation-order.txt"

  sealed trait Operation extends Serializable with Product
  object Operation {
    case class Num(value: Long) extends Operation
    case class Sum(l: Operation, r: Operation) extends Operation
    case class Multiply(l: Operation, r: Operation) extends Operation

    def eval(o: Operation): Long = o match {
      case Num(value)     => value
      case Sum(l, r)      => eval(l) + eval(r)
      case Multiply(l, r) => eval(l) * eval(r)
    }
  }

  def sumEvals(exprs: Seq[Operation]): Long = exprs.map(Operation.eval).sum

  sealed trait Part {

    def simple: Parser[Operation] = (
      "\\d+".r ^^ { value => Num(value.toLong) }
        | "(" ~> expr <~ ")"
    )

    def expr: Parser[Operation]

    def parseOperation(s: String): Operation = {
      parseAll(expr, s) match {
        case Success(result, next) => result
        case noSuccess: NoSuccess =>
          throw new RuntimeException(
            s"Operation parsing error: ${noSuccess.msg} (${noSuccess.next})"
          )
      }
    }

    def parseOperations(input: List[String]): Seq[Operation] =
      input.map(parseOperation).toSeq
  }

  object Part1 extends Part {

    def op: Parser[(Operation, Operation) => Operation] = (
      "+" ^^^ Sum
        | "*" ^^^ Operation.Multiply
    )

    def expr: Parser[Operation] = chainl1(simple, op)
  }

  object Part2 extends Part {

    def factor: Parser[Operation] = chainl1(simple, "+" ^^^ Sum)

    def expr: Parser[Operation] = chainl1(factor, "*" ^^^ Multiply)
  }

  val input = fileSource.toList

  println(sumEvals(Part1.parseOperations(input)))
  println(sumEvals(Part2.parseOperations(input)))

}
