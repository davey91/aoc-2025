import Day6.Arithmetic.{Addition, Multiply}

import java.time.Instant
import scala.io.Source

object Day6:
  val lines                  = Source.fromResource("day6/input.txt").getLines().toSeq
  val numberOrOperationRegex = """(\d{1,})\S*|(\+|\*)""".r

  enum Arithmetic:
    case Addition
    case Multiply
    case Number(value: BigInt)
    case Unknown
  object ValueAndIndex:
    def apply(raw: String, y: Int, x: Int): ValueAndIndex =
      raw match
        case "+"    => ValueAndIndex(Addition, y, x)
        case "*"    => ValueAndIndex(Multiply, y, x)
        case number => ValueAndIndex(Arithmetic.Number(BigInt(number)), y, x)

  case class ValueAndIndex(arithmetic: Arithmetic, y: Int, x: Int)
  object ArithmeticProblem:
    val empty = ArithmeticProblem(Nil, Arithmetic.Unknown)
  case class ArithmeticProblem(values: Seq[Arithmetic.Number], operation: Arithmetic):
    def accumulate: BigInt =
      val result: Arithmetic.Number = values.reduce((value, total) =>
        operation match
          case Arithmetic.Addition      => Arithmetic.Number(total.value + value.value)
          case Arithmetic.Multiply      => Arithmetic.Number(total.value * value.value)
          case Arithmetic.Number(value) => throw IllegalStateException("shouldn't happen")
      )
      //println(result)
      result.value

  def day6A(): BigInt =
    val valuesAndIndex = lines
      .map(raw => raw.trim.split(" ").toSeq)
      .zipWithIndex
      .flatMap((strings, y) =>
        strings
          .filterNot(_.isEmpty)
          .zipWithIndex
          .map((raw, x) =>
            val value = numberOrOperationRegex.findFirstIn(raw).getOrElse("")
            ValueAndIndex(value, y, x)
          )
      )

    val groupedByX: Iterable[Seq[ValueAndIndex]] = valuesAndIndex.groupBy(_.x).values
    val problems = groupedByX.map: values =>
        values.foldLeft(ArithmeticProblem.empty)((acc, value) =>
          value.arithmetic match
            case Arithmetic.Addition      => acc.copy(operation = Arithmetic.Addition)
            case Arithmetic.Multiply      => acc.copy(operation = Arithmetic.Multiply)
            case Arithmetic.Number(value) => acc.copy(values = acc.values :+ Arithmetic.Number(value))
            case Arithmetic.Unknown       => throw IllegalStateException("shouldn't happen")
        )
    .toSeq
    //problems.map(println)
    problems.map(_.accumulate).sum

  @main
  def six(): Unit =
    println(s"6A: ${day6A()}")
